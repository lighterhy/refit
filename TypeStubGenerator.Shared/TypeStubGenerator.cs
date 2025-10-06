namespace Refit.Generator;

using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

// * Search for all Interfaces, find the method definitions
//   and make sure there's at least one Refit attribute on one
// * Generate the data we need for the template based on interface method
//   defn's

/// <summary>
/// InterfaceStubGeneratorV2.
/// </summary>
[Generator]
#if ROSLYN_4
public class InterfaceStubGeneratorV2 : IIncrementalGenerator
#else
public class BaseTypeStubGenerator : ISourceGenerator
#endif
{
    private const string TypeParameterVariableName = "______typeParameters";

#if !ROSLYN_4

    /// <summary>
    /// Executes the specified context.
    /// </summary>
    /// <param name="context">The context.</param>
    public void Execute(GeneratorExecutionContext context)
    {
        if (context.SyntaxReceiver is not SyntaxReceiver receiver)
            return;

        context.AnalyzerConfigOptions.GlobalOptions.TryGetValue(
            "build_property.RefitInternalNamespace",
            out var refitInternalNamespace
        );

        var parseStep = Parser.GenerateBaseTypeStubs(
            (CSharpCompilation)context.Compilation,
            refitInternalNamespace,
            receiver.CandidateMethods.ToImmutableArray(),
            receiver.CandidateTypes.ToImmutableArray(),
            context.CancellationToken
        );

        // Emit diagnostics
        foreach (var diagnostic in parseStep.diagnostics)
        {
            context.ReportDiagnostic(diagnostic);
        }

        // Emit base type stubs
        foreach (var typeModel in parseStep.contextGenerationSpec.Types)
        {
            var baseTypeText = Emitter.EmitType(typeModel);
            context.AddSource(
                typeModel.FileName,
                baseTypeText
            );
        }

        // Emit PreserveAttribute and Generated.Initialize
        Emitter.EmitSharedCode(
            parseStep.contextGenerationSpec,
            (name, code) => context.AddSource(name, code)
        );
    }
#endif

#if ROSLYN_4

    /// <summary>
    /// Initializes the specified context.
    /// </summary>
    /// <param name="context">The context.</param>
    /// <returns></returns>
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        // We're looking for methods with an attribute that are in an interface or abstract class
        var candidateMethodsProvider = context.SyntaxProvider.CreateSyntaxProvider(
            (syntax, cancellationToken) =>
            {
                return syntax is MethodDeclarationSyntax { AttributeLists.Count: > 0 } methodSyntax &&
                    (methodSyntax.Parent is InterfaceDeclarationSyntax || GetAbstractClass(methodSyntax.Parent) is { });
            },
            (context, cancellationToken) => (MethodDeclarationSyntax)context.Node
        );

        // We also look for interfaces and abstract classes that derive from others, so we can see if any base methods contain
        // Refit methods
        var candidateBaseTypeProvider = context.SyntaxProvider.CreateSyntaxProvider(
            (syntax, cancellationToken) =>
                syntax is InterfaceDeclarationSyntax { BaseList: not null } || GetAbstractClass(syntax) is { BaseList: not null },
            (context, cancellationToken) => (TypeDeclarationSyntax)context.Node
        );

        var refitInternalNamespace = context.AnalyzerConfigOptionsProvider.Select(
            (analyzerConfigOptionsProvider, cancellationToken) =>
                analyzerConfigOptionsProvider.GlobalOptions.TryGetValue(
                    "build_property.RefitInternalNamespace",
                    out var refitInternalNamespace
                )
                    ? refitInternalNamespace
                    : null
        );

        var inputs = candidateMethodsProvider
            .Collect()
            .Combine(candidateBaseTypeProvider.Collect())
            .Select(
                (combined, cancellationToken) =>
                    (candidateMethods: combined.Left, candidateBaseTypes: combined.Right)
            )
            .Combine(refitInternalNamespace)
            .Combine(context.CompilationProvider)
            .Select(
                (combined, cancellationToken) =>
                    (
                        combined.Left.Left.candidateMethods,
                        combined.Left.Left.candidateBaseTypes,
                        refitInternalNamespace: combined.Left.Right,
                        compilation: combined.Right
                )
        );

        var parseStep = inputs.Select(
            (collectedValues, cancellationToken) =>
            {
                return Parser.GenerateBaseTypeStubs(
                    (CSharpCompilation)collectedValues.compilation,
                    collectedValues.refitInternalNamespace,
                    collectedValues.candidateMethods,
                    collectedValues.candidateBaseTypes,
                    cancellationToken
                );
            }
        );

        // output the diagnostics
        // use `ImmutableEquatableArray` to cache cases where there are no diagnostics
        // otherwise the subsequent steps will always rerun.
        var diagnostics = parseStep
            .Select(static (x, _) => x.diagnostics.ToImmutableEquatableArray())
            .WithTrackingName(RefitGeneratorStepName.ReportDiagnostics);
        context.ReportDiagnostics(diagnostics);

        var contextModel = parseStep.Select(static (x, _) => x.Item2);
        var interfaceModels = contextModel
            .SelectMany(static (x, _) => x.Types)
            .WithTrackingName(RefitGeneratorStepName.BuildRefit);
        context.EmitSource(interfaceModels);

        context.RegisterImplementationSourceOutput(
            contextModel,
            static (spc, model) => Emitter.EmitSharedCode(model, (name, code) => spc.AddSource(name, code)));
    }

#else

    /// <summary>
    /// Initializes the specified context.
    /// </summary>
    /// <param name="context">The context.</param>
    /// <returns></returns>
    public void Initialize(GeneratorInitializationContext context)
    {
        context.RegisterForSyntaxNotifications(() => new SyntaxReceiver());
    }

    class SyntaxReceiver : ISyntaxReceiver
    {
        public List<MethodDeclarationSyntax> CandidateMethods { get; } = [];

        public List<TypeDeclarationSyntax> CandidateTypes { get; } = [];

        public void OnVisitSyntaxNode(SyntaxNode syntaxNode)
        {
            // We're looking for methods with an attribute that are in an interfaces or abstract classes
            if (
                syntaxNode is MethodDeclarationSyntax { AttributeLists.Count: > 0 } methodDeclarationSyntax &&
                    (methodDeclarationSyntax.Parent is InterfaceDeclarationSyntax || GetAbstractClass(methodDeclarationSyntax.Parent) is { })
            )
            {
                CandidateMethods.Add(methodDeclarationSyntax);
            }

            // We also look for interfaces or abstract classes that derive from others, so we can see if any base methods contain
            // Refit methods

            if (syntaxNode is InterfaceDeclarationSyntax { BaseList: not null } ||
                GetAbstractClass(syntaxNode) is { BaseList: not null })
            {
                CandidateTypes.Add((TypeDeclarationSyntax)syntaxNode);
            }
        }
    }

#endif

    private static ClassDeclarationSyntax? GetAbstractClass(SyntaxNode? syntax)
    {
        return syntax is ClassDeclarationSyntax classDeclarationSyntax &&
            classDeclarationSyntax.Modifiers.Any(SyntaxKind.AbstractKeyword)
            ? classDeclarationSyntax
            : null;
    }

    internal static class RefitGeneratorStepName
    {
        public const string ReportDiagnostics = "ReportDiagnostics";
        public const string BuildRefit = "BuildRefit";
    }
}
