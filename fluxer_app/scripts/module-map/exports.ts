import {parse} from '@babel/parser';
import * as t from '@babel/types';
import type {Compilation} from '@rspack/core';

type CompilationModule = Parameters<Compilation['chunkGraph']['getModuleId']>[0];

interface GeneratedExport {
	name: string;
	binding?: string;
	isDefaultBinding: boolean;
}

type AstNode = t.Node;

const parseModuleSource = (code: string) =>
	parse(code, {
		sourceType: 'unambiguous',
		allowReturnOutsideFunction: true,
		plugins: ['jsx', 'typescript'],
	});

const getExportName = (node: AstNode | null | undefined): string | undefined => {
	if (t.isIdentifier(node)) return node.name;
	if (t.isStringLiteral(node)) return node.value;
	return undefined;
};

const getObjectPropertyName = (property: t.ObjectProperty): string | undefined => {
	const key = property.key;

	if (t.isIdentifier(key)) return key.name;
	if (t.isStringLiteral(key)) return key.value;
	if (t.isNumericLiteral(key)) return String(key.value);

	return undefined;
};

const getGeneratedBinding = (node: AstNode | null | undefined): string | undefined => {
	if (t.isIdentifier(node)) return node.name;

	if (t.isArrowFunctionExpression(node)) {
		if (t.isBlockStatement(node.body)) {
			const [statement] = node.body.body;

			if (node.body.body.length === 1 && t.isReturnStatement(statement)) {
				return getGeneratedBinding(statement.argument);
			}

			return undefined;
		}

		return getGeneratedBinding(node.body);
	}

	return undefined;
};

const hasDefaultBindingComment = (node: t.ObjectProperty): boolean => {
	const comments = [...(node.leadingComments ?? []), ...(node.value.leadingComments ?? [])];

	return comments.some((comment) => comment.value.includes('export default binding'));
};

const getGeneratedExports = (code: string): Array<GeneratedExport> => {
	const ast = parseModuleSource(code);
	const exports: Array<GeneratedExport> = [];

	const visit = (value: unknown): void => {
		if (Array.isArray(value)) {
			for (const item of value) {
				visit(item);
			}
			return;
		}

		if (typeof value !== 'object' || value === null || !('type' in value)) {
			return;
		}

		const node = value as AstNode;

		if (t.isCallExpression(node)) {
			const {callee, arguments: args} = node;

			if (
				t.isMemberExpression(callee) &&
				t.isIdentifier(callee.object, {name: '__webpack_require__'}) &&
				t.isIdentifier(callee.property, {name: 'd'})
			) {
				for (const argument of args.slice(1, 3)) {
					if (!t.isObjectExpression(argument)) continue;

					for (const property of argument.properties) {
						if (!t.isObjectProperty(property)) continue;

						const name = getObjectPropertyName(property);
						if (name === undefined) continue;

						exports.push({
							name,
							binding: getGeneratedBinding(property.value),
							isDefaultBinding: hasDefaultBindingComment(property),
						});
					}
				}
			}
		}

		for (const child of Object.values(node)) visit(child);
	};

	visit(ast);

	return exports;
};

const collectExportBindings = (code: string): Map<string, Set<string>> => {
	const ast = parseModuleSource(code);
	const bindings = new Map<string, Set<string>>();

	const addBinding = (local: string | undefined, exported: string | undefined) => {
		if (!local || !exported) {
			return;
		}

		const exports = bindings.get(local) ?? new Set<string>();
		exports.add(exported);
		bindings.set(local, exports);
	};

	const addPatternBindings = (pattern: AstNode | null | undefined): void => {
		if (t.isIdentifier(pattern)) {
			addBinding(pattern.name, pattern.name);
		} else if (t.isRestElement(pattern)) {
			addPatternBindings(pattern.argument);
		} else if (t.isAssignmentPattern(pattern)) {
			addPatternBindings(pattern.left);
		} else if (t.isObjectPattern(pattern)) {
			for (const property of pattern.properties) {
				if (t.isRestElement(property)) {
					addPatternBindings(property.argument);
				} else if (t.isObjectProperty(property)) {
					addPatternBindings(property.value);
				}
			}
		} else if (t.isArrayPattern(pattern)) {
			for (const element of pattern.elements) {
				addPatternBindings(element);
			}
		}
	};

	for (const statement of ast.program.body) {
		if (t.isExportNamedDeclaration(statement) && statement.source == null) {
			const declaration = statement.declaration;

			if (t.isFunctionDeclaration(declaration) || t.isClassDeclaration(declaration)) {
				if (declaration.id) {
					addBinding(declaration.id.name, declaration.id.name);
				}
			} else if (t.isVariableDeclaration(declaration)) {
				for (const declarator of declaration.declarations) {
					addPatternBindings(declarator.id);
				}
			}

			for (const specifier of statement.specifiers) {
				if (!t.isExportSpecifier(specifier)) continue;

				addBinding(getExportName(specifier.local), getExportName(specifier.exported));
			}
		} else if (t.isExportDefaultDeclaration(statement)) {
			const declaration = statement.declaration;

			if ((t.isFunctionDeclaration(declaration) || t.isClassDeclaration(declaration)) && declaration.id) {
				addBinding(declaration.id.name, 'default');
			}
		}
	}

	return bindings;
};

const mapGeneratedExports = (
	generatedExports: Array<GeneratedExport>,
	exportNames: Array<string>,
	sourceBindings: Map<string, Set<string>>,
): Record<string, string> | undefined => {
	const result: Record<string, string> = {};
	const mappedGeneratedNames = new Set<string>();

	for (const generated of generatedExports) {
		let originalName: string | undefined;

		if (exportNames.includes(generated.name)) {
			originalName = generated.name;
		} else if (generated.isDefaultBinding && exportNames.includes('default')) {
			originalName = 'default';
		} else if (generated.binding) {
			const candidates = [...(sourceBindings.get(generated.binding) ?? [])].filter((name) =>
				exportNames.includes(name),
			);

			if (candidates.length === 1) {
				originalName = candidates[0];
			}
		}

		if (originalName) {
			result[originalName] = generated.name;
			mappedGeneratedNames.add(generated.name);
		}
	}

	const unmappedOriginalNames = exportNames.filter((name) => !(name in result));
	const unmappedGenerated = generatedExports.filter((generated) => !mappedGeneratedNames.has(generated.name));

	if (unmappedOriginalNames.length === 1 && unmappedGenerated.length === 1) {
		result[unmappedOriginalNames[0]] = unmappedGenerated[0].name;
	}

	return Object.keys(result).length === exportNames.length ? result : undefined;
};

export const getModuleExportMap = (
	compilation: Compilation,
	module: CompilationModule,
): Record<string, string> | undefined => {
	const runtimes = [
		...new Set([...compilation.chunkGraph.getModuleChunksIterable(module)].flatMap((chunk) => [...chunk.runtime])),
	];

	if (!runtimes.length) return undefined;

	const codeGenerationRuntime =
		runtimes.find((runtime) => compilation.entries.has(runtime)) ?? runtimes[runtimes.length - 1];

	if (!codeGenerationRuntime) return undefined;

	const usedExports = compilation.moduleGraph.getUsedExports(module, runtimes);
	const providedExports = compilation.moduleGraph.getProvidedExports(module);

	let exportNames: Array<string> | null = null;

	if (Array.isArray(usedExports)) {
		exportNames = usedExports;
	} else if (Array.isArray(providedExports)) {
		if (usedExports === true) {
			exportNames = providedExports;
		}
		if (usedExports === false) {
			exportNames = [];
		}
	}

	if (!exportNames?.length) return undefined;

	const source = compilation.codeGenerationResults.get(module, codeGenerationRuntime).sources.get('javascript');

	if (!source) return undefined;

	const generatedExports = getGeneratedExports(source.source().toString());

	if (generatedExports.length !== exportNames.length) return undefined;

	const originalSource =
		typeof module.type === 'string' && module.type.startsWith('javascript/') ? module.originalSource() : null;

	const sourceBindings = originalSource
		? collectExportBindings(originalSource.source().toString())
		: new Map<string, Set<string>>();

	return mapGeneratedExports(generatedExports, exportNames, sourceBindings);
};
