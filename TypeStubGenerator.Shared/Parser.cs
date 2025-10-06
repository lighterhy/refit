namespace Refit.Generator;

using System.Collections.Immutable;
using System.Text;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

internal static class Parser
{
    /// <summary>
    /// Generates the interface stubs.
    /// </summary>
    /// <param name="compilation">The compilation.</param>
    /// <param name="refitInternalNamespace">The refit internal namespace.</param>
    /// <param name="candidateMethods">The candidate methods.</param>
    /// <param name="candidateBaseTypes">The candidate interfaces.</param>
    /// <param name="cancellationToken">The cancellation token.</param>
    /// <returns></returns>
    public static (
        List<Diagnostic> diagnostics,
        ContextGenerationModel contextGenerationSpec
    ) GenerateBaseTypeStubs(
        CSharpCompilation compilation,
        string? refitInternalNamespace,
        ImmutableArray<MethodDeclarationSyntax> candidateMethods,
        ImmutableArray<TypeDeclarationSyntax> candidateBaseTypes,
        CancellationToken cancellationToken
    )
    {
        if (compilation == null)
            throw new ArgumentNullException(nameof(compilation));

        var wellKnownTypes = new WellKnownTypes(compilation);

        refitInternalNamespace = $"{refitInternalNamespace ?? string.Empty}RefitInternalGenerated";

        // Remove - as they are valid in csproj, but invalid in a namespace
        refitInternalNamespace = refitInternalNamespace.Replace('-', '_').Replace('@', '_');

        // we're going to create a new compilation that contains the attribute.
        // TODO: we should allow source generators to provide source during initialize, so that this step isn't required.
        var options = (CSharpParseOptions)compilation.SyntaxTrees[0].Options;

        var disposableInterfaceSymbol = wellKnownTypes.Get(typeof(IDisposable));
        var httpMethodBaseAttributeSymbol = wellKnownTypes.TryGet(
            "Refit.HttpMethodAttribute"
        );

        var diagnostics = new List<Diagnostic>();
        if (httpMethodBaseAttributeSymbol == null)
        {
            diagnostics.Add(Diagnostic.Create(DiagnosticDescriptors.RefitNotReferenced, null));
            return (
                diagnostics,
                new ContextGenerationModel(
                    refitInternalNamespace,
                    string.Empty,
                    ImmutableEquatableArray.Empty<TypeModel>()
                )
            );
        }

        // Check the candidates and keep the ones we're actually interested in

#pragma warning disable RS1024 // Compare symbols correctly
        var interfaceToNullableEnabledMap = new Dictionary<INamedTypeSymbol, bool>(
            SymbolEqualityComparer.Default
        );
#pragma warning restore RS1024 // Compare symbols correctly
        var methodSymbols = new List<IMethodSymbol>();
        foreach (var group in candidateMethods.GroupBy(m => m.SyntaxTree))
        {
            var model = compilation.GetSemanticModel(group.Key);
            foreach (var method in group)
            {
                // Get the symbol being declared by the method
                var methodSymbol = model.GetDeclaredSymbol(
                    method,
                    cancellationToken: cancellationToken
                );
                if (!IsRefitMethod(methodSymbol, httpMethodBaseAttributeSymbol))
                    continue;

                var isAnnotated =
                    compilation.Options.NullableContextOptions == NullableContextOptions.Enable
                    || model.GetNullableContext(method.SpanStart) == NullableContext.Enabled;
                interfaceToNullableEnabledMap[methodSymbol!.ContainingType] = isAnnotated;

                methodSymbols.Add(methodSymbol!);
            }
        }

        var types = methodSymbols
            .GroupBy<IMethodSymbol, INamedTypeSymbol>(
                m => m.ContainingType,
                SymbolEqualityComparer.Default
            )
            .ToDictionary<
                IGrouping<INamedTypeSymbol, IMethodSymbol>,
                INamedTypeSymbol,
                List<IMethodSymbol>
            >(g => g.Key, v => [.. v], SymbolEqualityComparer.Default);

        // Look through the candidate types
        var interfaceSymbols = new List<INamedTypeSymbol>();
        foreach (var group in candidateBaseTypes.GroupBy(i => i.SyntaxTree))
        {
            var model = compilation.GetSemanticModel(group.Key);
            foreach (var type in group)
            {
                // get the symbol belonging to the types
                var typeSymbol = model.GetDeclaredSymbol(
                    type,
                    cancellationToken: cancellationToken
                );

                // See if we already know about it, might be a dup
                if (typeSymbol is null || types.ContainsKey(typeSymbol))
                    continue;

                // The type has no refit methods, but its base interfaces might
                var hasDerivedRefit = typeSymbol.TypeKind switch
                {
                    TypeKind.Interface => typeSymbol
                        .AllInterfaces.SelectMany(i => i.GetMembers().OfType<IMethodSymbol>())
                        .Any(m => IsRefitMethod(m, httpMethodBaseAttributeSymbol)),

                    TypeKind.Class when typeSymbol.IsAbstract => typeSymbol
                        .GetBaseTypesAndThis().Where(a => !a.Equals(typeSymbol, SymbolEqualityComparer.Default))
                        .SelectMany(i => i.GetMembers().OfType<IMethodSymbol>())
                        .Any(m => IsRefitMethod(m, httpMethodBaseAttributeSymbol)),

                    _ => false
                };

                if (hasDerivedRefit)
                {
                    // Add the interface to the generation list with an empty set of methods
                    // The logic already looks for base refit methods
                    types.Add(typeSymbol, []);
                    var isAnnotated =
                        model.GetNullableContext(type.SpanStart) == NullableContext.Enabled;

                    interfaceToNullableEnabledMap[typeSymbol] = isAnnotated;
                }
            }
        }

        cancellationToken.ThrowIfCancellationRequested();

        // Bail out if there aren't any interfaces to generate code for. This may be the case with transitives
        if (types.Count == 0)
            return (
                diagnostics,
                new ContextGenerationModel(
                    refitInternalNamespace,
                    string.Empty,
                    ImmutableEquatableArray.Empty<TypeModel>()
                )
            );

        var supportsNullable = options.LanguageVersion >= LanguageVersion.CSharp8;

        var keyCount = new Dictionary<string, int>(StringComparer.OrdinalIgnoreCase);

        var attributeText =
            @$"
#pragma warning disable
namespace {refitInternalNamespace}
{{
    [global::System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage]
    [global::System.ComponentModel.EditorBrowsable(global::System.ComponentModel.EditorBrowsableState.Never)]
    [global::System.AttributeUsage (global::System.AttributeTargets.Class | global::System.AttributeTargets.Struct | global::System.AttributeTargets.Enum | global::System.AttributeTargets.Constructor | global::System.AttributeTargets.Method | global::System.AttributeTargets.Property | global::System.AttributeTargets.Field | global::System.AttributeTargets.Event | global::System.AttributeTargets.Interface | global::System.AttributeTargets.Delegate)]
    sealed class PreserveAttribute : global::System.Attribute
    {{
        //
        // Fields
        //
        public bool AllMembers;

        public bool Conditional;
    }}
}}
#pragma warning restore
";

        // TODO: Delete?
        // Is it necessary to add the attributes to the compilation now, does it affect the users ide experience?
        // Is it needed in order to get the preserve attribute display name.
        // Will the compilation ever change this.
        compilation = compilation.AddSyntaxTrees(
            CSharpSyntaxTree.ParseText(
                SourceText.From(attributeText, Encoding.UTF8),
                options,
                cancellationToken: cancellationToken
            )
        );

        // get the newly bound attribute
        var preserveAttributeSymbol = compilation.GetTypeByMetadataName(
            $"{refitInternalNamespace}.PreserveAttribute"
        )!;

        var preserveAttributeDisplayName = preserveAttributeSymbol.ToDisplayString(
            SymbolDisplayFormat.FullyQualifiedFormat
        );

        var baseTypeModels = new List<TypeModel>();
        // group the fields by interface and generate the source
        foreach (var group in types)
        {
            cancellationToken.ThrowIfCancellationRequested();

            // each group is keyed by the Interface INamedTypeSymbol and contains the members
            // with a refit attribute on them. Types may contain other members, without the attribute, which we'll
            // need to check for and error out on
            var keyName = group.Key.Name;
            int value;
            while (keyCount.TryGetValue(keyName, out value))
            {
                keyName = $"{keyName}{++value}";
            }
            keyCount[keyName] = value;
            var fileName = $"{keyName}.g.cs";

            var typeModel = ProcessBaseType(
                fileName,
                diagnostics,
                group.Key,
                group.Value,
                preserveAttributeDisplayName,
                disposableInterfaceSymbol,
                httpMethodBaseAttributeSymbol,
                supportsNullable,
                interfaceToNullableEnabledMap[group.Key]
            );

            baseTypeModels.Add(typeModel);
        }

        var contextGenerationSpec = new ContextGenerationModel(
            refitInternalNamespace,
            preserveAttributeDisplayName,
            baseTypeModels.ToImmutableEquatableArray()
        );
        return (diagnostics, contextGenerationSpec);
    }

    static TypeModel ProcessBaseType(
        string fileName,
        List<Diagnostic> diagnostics,
        INamedTypeSymbol baseTypeSymbol,
        List<IMethodSymbol> refitMethods,
        string preserveAttributeDisplayName,
        ISymbol disposableBaseTypeSymbol,
        INamedTypeSymbol httpMethodBaseAttributeSymbol,
        bool supportsNullable,
        bool nullableEnabled
    )
    {
        // Get the class name with the type parameters, then remove the namespace
        var className = baseTypeSymbol.ToDisplayString();
        var lastDot = className.LastIndexOf('.');
        if (lastDot > 0)
        {
            className = className.Substring(lastDot + 1);
        }
        var classDeclaration = $"{baseTypeSymbol.ContainingType?.Name}{className}";

        // Get the class name itself
        var classSuffix = $"{baseTypeSymbol.ContainingType?.Name}{baseTypeSymbol.Name}";
        var ns = baseTypeSymbol.ContainingNamespace?.ToDisplayString();

        // if it's the global namespace, our lookup rules say it should be the same as the class name
        if (baseTypeSymbol.ContainingNamespace is { IsGlobalNamespace: true })
        {
            ns = string.Empty;
        }

        // Remove dots
        ns = ns!.Replace(".", "");
        var interfaceDisplayName = baseTypeSymbol.ToDisplayString(
            SymbolDisplayFormat.FullyQualifiedFormat
        );

        // Get any other methods on the refit interfaces. We'll need to generate something for them and warn
        var nonRefitMethods = baseTypeSymbol
            .GetMembers()
            .OfType<IMethodSymbol>()
            .Except(refitMethods, SymbolEqualityComparer.Default)
            .Cast<IMethodSymbol>()
            .ToArray();

        // get methods for all inherited
        var derivedMethods = baseTypeSymbol.TypeKind switch
        {
            TypeKind.Interface => baseTypeSymbol
                .AllInterfaces.SelectMany(i => i.GetMembers().OfType<IMethodSymbol>())
                .ToList(),
            TypeKind.Class when baseTypeSymbol.IsAbstract => baseTypeSymbol
                .GetBaseTypesAndThis().Where(a => !a.Equals(baseTypeSymbol, SymbolEqualityComparer.Default))
                .SelectMany(i => i.GetMembers().OfType<IMethodSymbol>())
                .ToList(),
            _ => [],
        };

        // Look for disposable
        var disposeMethod = derivedMethods.Find(
            m =>
                m.ContainingType?.Equals(disposableBaseTypeSymbol, SymbolEqualityComparer.Default)
                == true
        );
        if (disposeMethod != null)
        {
            //remove it from the derived methods list so we don't process it with the rest
            derivedMethods.Remove(disposeMethod);
        }

        // Pull out the refit methods from the derived types
        var derivedRefitMethods = derivedMethods
            .Where(m => IsRefitMethod(m, httpMethodBaseAttributeSymbol))
            .ToArray();
        var derivedNonRefitMethods = derivedMethods
            .Except(derivedRefitMethods, SymbolEqualityComparer.Default)
            .Cast<IMethodSymbol>()
            .ToArray();

        var memberNames = baseTypeSymbol
            .GetMembers()
            .Select(x => x.Name)
            .Distinct()
            .ToImmutableEquatableArray();

        // Handle Refit Methods
        var refitMethodsArray = refitMethods
            .Select(m => ParseMethod(m, true))
            .ToImmutableEquatableArray();

        var derivedRefitMethodsArray = baseTypeSymbol.TypeKind is TypeKind.Class
            ? ImmutableEquatableArray<MethodModel>.Empty
            : refitMethods
                .Concat(derivedRefitMethods)
                .Select(m => ParseMethod(m, false))
                .ToImmutableEquatableArray();

        // Handle non-refit Methods that aren't static or properties or have a method body
        var nonRefitMethodModelList = new List<MethodModel>();
        foreach (var method in nonRefitMethods.Concat(derivedNonRefitMethods))
        {
            if (
                method.IsStatic
                || method.MethodKind == MethodKind.PropertyGet
                || method.MethodKind == MethodKind.PropertySet
                || !method.IsAbstract
            ) // If an interface method has a body, it won't be abstract
                continue;

            nonRefitMethodModelList.Add(ParseNonRefitMethod(method, diagnostics));
        }

        var nonRefitMethodModels = nonRefitMethodModelList.ToImmutableEquatableArray();

        var constraints = GenerateConstraints(baseTypeSymbol.TypeParameters, false);
        var hasDispose = disposeMethod != null;
        var nullability = (supportsNullable, nullableEnabled) switch
        {
            (false, _) => Nullability.None,
            (true, true) => Nullability.Enabled,
            (true, false) => Nullability.Disabled,
        };
        return new TypeModel(
            preserveAttributeDisplayName,
            fileName,
            className,
            ns,
            classDeclaration,
            interfaceDisplayName,
            classSuffix,
            constraints,
            memberNames,
            nonRefitMethodModels,
            refitMethodsArray,
            derivedRefitMethodsArray,
            nullability,
            hasDispose
        );
    }

    private static MethodModel ParseNonRefitMethod(
        IMethodSymbol methodSymbol,
        List<Diagnostic> diagnostics
    )
    {
        // report invalid error diagnostic
        foreach (var location in methodSymbol.Locations)
        {
            var diagnostic = Diagnostic.Create(
                DiagnosticDescriptors.InvalidRefitMember,
                location,
                methodSymbol.ContainingType.Name,
                methodSymbol.Name
            );
            diagnostics.Add(diagnostic);
        }

        return ParseMethod(methodSymbol, false);
    }

    private static bool IsRefitMethod(
        IMethodSymbol? methodSymbol,
        INamedTypeSymbol httpMethodAttribute
    )
    {
        return methodSymbol
                ?.GetAttributes()
                .Any(ad => ad.AttributeClass?.InheritsFromOrEquals(httpMethodAttribute) == true)
            == true;
    }

    private static ImmutableEquatableArray<TypeConstraint> GenerateConstraints(
        ImmutableArray<ITypeParameterSymbol> typeParameters,
        bool isOverrideOrExplicitImplementation
    )
    {
        // Need to loop over the constraints and create them
        return typeParameters
            .Select(
                typeParameter =>
                    ParseConstraintsForTypeParameter(
                        typeParameter,
                        isOverrideOrExplicitImplementation
                    )
            )
            .ToImmutableEquatableArray();
    }

    private static TypeConstraint ParseConstraintsForTypeParameter(
        ITypeParameterSymbol typeParameter,
        bool isOverrideOrExplicitImplementation
    )
    {
        // Explicit interface implementations and overrides can only have class or struct constraints
        var known = KnownTypeConstraint.None;

        if (typeParameter.HasReferenceTypeConstraint)
        {
            known |= KnownTypeConstraint.Class;
        }
        if (typeParameter.HasUnmanagedTypeConstraint && !isOverrideOrExplicitImplementation)
        {
            known |= KnownTypeConstraint.Unmanaged;
        }

        // unmanaged constraints are both structs and unmanaged so the struct constraint is redundant
        if (typeParameter.HasValueTypeConstraint && !typeParameter.HasUnmanagedTypeConstraint)
        {
            known |= KnownTypeConstraint.Struct;
        }
        if (typeParameter.HasNotNullConstraint && !isOverrideOrExplicitImplementation)
        {
            known |= KnownTypeConstraint.NotNull;
        }

        var constraints = ImmutableEquatableArray<string>.Empty;
        if (!isOverrideOrExplicitImplementation)
        {
            constraints = typeParameter
                .ConstraintTypes.Select(
                    typeConstraint =>
                        typeConstraint.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)
                )
                .ToImmutableEquatableArray();
        }

        // new constraint has to be last
        if (typeParameter.HasConstructorConstraint && !isOverrideOrExplicitImplementation)
        {
            known |= KnownTypeConstraint.New;
        }

        var declaredName = typeParameter.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
        return new TypeConstraint(typeParameter.Name, declaredName, known, constraints);
    }

    private static ParameterModel ParseParameter(IParameterSymbol param)
    {
        var annotation =
            !param.Type.IsValueType && param.NullableAnnotation == NullableAnnotation.Annotated;

        var paramType = param.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
        var isGeneric = ContainsTypeParameter(param.Type);

        return new ParameterModel(param.MetadataName, paramType, annotation, isGeneric);
    }

    private static bool ContainsTypeParameter(ITypeSymbol symbol)
    {
        if (symbol is ITypeParameterSymbol)
            return true;

        if (symbol is not INamedTypeSymbol { TypeParameters.Length: > 0 } namedType)
            return false;

        foreach (var typeArg in namedType.TypeArguments)
        {
            if (ContainsTypeParameter(typeArg))
                return true;
        }

        return false;
    }

    private static MethodModel ParseMethod(IMethodSymbol methodSymbol, bool isImplicit)
    {
        var returnType = methodSymbol.ReturnType.ToDisplayString(
            SymbolDisplayFormat.FullyQualifiedFormat
        );

        var containingType = methodSymbol.ContainingType.ToDisplayString(
            SymbolDisplayFormat.FullyQualifiedFormat
        );
        var declaredMethod = methodSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
        var returnTypeInfo = methodSymbol.ReturnType.MetadataName switch
        {
            "Task" => ReturnTypeInfo.AsyncVoid,
            "Task`1" or "ValueTask`1" => ReturnTypeInfo.AsyncResult,
            _ => ReturnTypeInfo.Return,
        };

        var parameters = methodSymbol.Parameters.Select(ParseParameter).ToImmutableEquatableArray();

        var constraints = GenerateConstraints(methodSymbol.TypeParameters, !isImplicit);

        return new MethodModel(
            methodSymbol.Name,
            returnType,
            containingType,
            declaredMethod,
            returnTypeInfo,
            methodSymbol.ContainingType.TypeKind is TypeKind.Class,
            parameters,
            constraints
        );
    }
}
