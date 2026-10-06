// SPDX-License-Identifier: AGPL-3.0-or-later
import * as path from 'node:path';
import {type BinaryExpression, type Identifier, Node, type Project, type SourceFile, SyntaxKind, ts} from 'ts-morph';

export const UNRESOLVED = Symbol('unresolved');

const APP_ALIAS_PREFIX = '@app/';

type StaticPrimitive = string | number | boolean | null;

export type StaticValue =
	| StaticPrimitive
	| ReadonlyArray<StaticValue>
	| {readonly [key: string]: StaticValue}
	| typeof UNRESOLVED;

export type Scope = ReadonlyMap<Node, StaticValue>;

export const EMPTY_SCOPE: Scope = new Map<Node, StaticValue>();

const MAX_DEPTH = 48;

function isPlainObject(value: StaticValue): value is {readonly [key: string]: StaticValue} {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function isPrimitive(value: StaticValue): value is StaticPrimitive {
	return value === null || typeof value === 'string' || typeof value === 'number' || typeof value === 'boolean';
}

function unwrap(node: Node): Node {
	let current = node;
	while (
		Node.isParenthesizedExpression(current) ||
		Node.isAsExpression(current) ||
		Node.isSatisfiesExpression(current) ||
		Node.isNonNullExpression(current) ||
		Node.isTypeAssertion(current)
	) {
		current = current.getExpression();
	}
	return current;
}

export function getIdentifierDeclarations(node: Identifier): ReadonlyArray<Node> {
	const parent = node.getParent();
	const symbol =
		Node.isShorthandPropertyAssignment(parent) && parent.getNameNode() === node
			? parent.getValueSymbol()
			: node.getSymbol();
	return symbol?.getDeclarations() ?? [];
}

export class StaticPathResolver {
	private readonly inFlight = new Set<Node>();
	private readonly appSourceRoots = new Map<string, string | null>();

	constructor(private readonly project: Project) {}

	public resolve(node: Node, scope: Scope): StaticValue {
		return this.evaluate(node, scope, 0);
	}

	public resolveString(node: Node, scope: Scope): string | null {
		const value = this.evaluate(node, scope, 0);
		return typeof value === 'string' ? value : null;
	}

	public resolvePropertyName(node: Node, scope: Scope): string | null {
		const name = this.evaluatePropertyName(node, scope, 0);
		return name === UNRESOLVED ? null : name;
	}

	private evaluatePropertyName(node: Node, scope: Scope, depth: number): string | typeof UNRESOLVED {
		if (Node.isIdentifier(node)) return node.getText();
		const expression = Node.isComputedPropertyName(node) ? node.getExpression() : node;
		const value = this.evaluate(expression, scope, depth + 1);
		return isPrimitive(value) ? String(value) : UNRESOLVED;
	}

	private evaluate(rawNode: Node, scope: Scope, depth: number): StaticValue {
		if (depth > MAX_DEPTH) {
			return UNRESOLVED;
		}
		const node = unwrap(rawNode);
		if (Node.isStringLiteral(node) || Node.isNoSubstitutionTemplateLiteral(node)) {
			return node.getLiteralValue();
		}
		if (Node.isNumericLiteral(node)) {
			return node.getLiteralValue();
		}
		if (Node.isTrueLiteral(node)) {
			return true;
		}
		if (Node.isFalseLiteral(node)) {
			return false;
		}
		if (Node.isNullLiteral(node)) {
			return null;
		}
		if (Node.isTemplateExpression(node)) {
			let text = node.getHead().getLiteralText();
			for (const span of node.getTemplateSpans()) {
				const value = this.evaluate(span.getExpression(), scope, depth + 1);
				if (!isPrimitive(value)) {
					return UNRESOLVED;
				}
				text += String(value);
				text += span.getLiteral().getLiteralText();
			}
			return text;
		}
		if (Node.isIdentifier(node)) {
			return this.evaluateIdentifier(node, scope, depth);
		}
		if (Node.isPropertyAccessExpression(node)) {
			const target = this.evaluate(node.getExpression(), scope, depth + 1);
			if (!isPlainObject(target)) {
				return UNRESOLVED;
			}
			const name = node.getName();
			return Object.hasOwn(target, name) ? target[name] : UNRESOLVED;
		}
		if (Node.isElementAccessExpression(node)) {
			const target = this.evaluate(node.getExpression(), scope, depth + 1);
			const argument = node.getArgumentExpression();
			if (argument == null || target === UNRESOLVED) {
				return UNRESOLVED;
			}
			const key = this.evaluate(argument, scope, depth + 1);
			if (typeof key !== 'string' && typeof key !== 'number') {
				return UNRESOLVED;
			}
			if (Array.isArray(target)) {
				const index = Number(key);
				if (typeof key === 'string' && String(index) !== key) return UNRESOLVED;
				return Number.isInteger(index) && index >= 0 && index < target.length ? target[index] : UNRESOLVED;
			}
			if (isPlainObject(target)) {
				const name = String(key);
				return Object.hasOwn(target, name) ? target[name] : UNRESOLVED;
			}
			return UNRESOLVED;
		}
		if (Node.isArrayLiteralExpression(node)) {
			const values: Array<StaticValue> = [];
			for (const element of node.getElements()) {
				const value = this.evaluate(
					Node.isSpreadElement(element) ? element.getExpression() : element,
					scope,
					depth + 1,
				);
				if (value === UNRESOLVED) {
					return UNRESOLVED;
				}
				if (Node.isSpreadElement(element)) {
					if (!Array.isArray(value)) return UNRESOLVED;
					values.push(...value);
				} else {
					values.push(value);
				}
			}
			return values;
		}
		if (Node.isObjectLiteralExpression(node)) {
			const properties = new Map<string, StaticValue>();
			for (const property of node.getProperties()) {
				if (!Node.isPropertyAssignment(property) && !Node.isShorthandPropertyAssignment(property)) return UNRESOLVED;
				const nameNode = property.getNameNode();
				const name = this.evaluatePropertyName(nameNode, scope, depth + 1);
				if (name === UNRESOLVED) return UNRESOLVED;
				if (Node.isPropertyAssignment(property) && name === '__proto__' && !Node.isComputedPropertyName(nameNode)) {
					return UNRESOLVED;
				}
				const initializer = Node.isPropertyAssignment(property) ? property.getInitializer() : nameNode;
				if (initializer === undefined) return UNRESOLVED;
				properties.set(name, this.evaluate(initializer, scope, depth + 1));
			}
			return Object.fromEntries(properties);
		}
		if (Node.isConditionalExpression(node)) {
			const condition = this.evaluate(node.getCondition(), scope, depth + 1);
			if (condition === UNRESOLVED) {
				return UNRESOLVED;
			}
			return this.evaluate(condition ? node.getWhenTrue() : node.getWhenFalse(), scope, depth + 1);
		}
		if (Node.isPrefixUnaryExpression(node)) {
			if (node.getOperatorToken() !== SyntaxKind.ExclamationToken) {
				return UNRESOLVED;
			}
			const operand = this.evaluate(node.getOperand(), scope, depth + 1);
			return operand === UNRESOLVED ? UNRESOLVED : !operand;
		}
		if (Node.isBinaryExpression(node)) {
			return this.evaluateBinary(node, scope, depth);
		}
		return UNRESOLVED;
	}

	private evaluateBinary(node: BinaryExpression, scope: Scope, depth: number): StaticValue {
		const operator = node.getOperatorToken().getText();
		const left = this.evaluate(node.getLeft(), scope, depth + 1);
		if (left === UNRESOLVED) {
			return UNRESOLVED;
		}
		if (operator === '&&') {
			return left ? this.evaluate(node.getRight(), scope, depth + 1) : left;
		}
		if (operator === '||') {
			return left || this.evaluate(node.getRight(), scope, depth + 1);
		}
		if (operator === '??') {
			return left === null ? this.evaluate(node.getRight(), scope, depth + 1) : left;
		}
		const right = this.evaluate(node.getRight(), scope, depth + 1);
		if (right === UNRESOLVED) {
			return UNRESOLVED;
		}
		if (!isPrimitive(left) || !isPrimitive(right)) return UNRESOLVED;
		switch (operator) {
			case '+':
				return typeof left === 'string' || typeof right === 'string'
					? String(left) + String(right)
					: Number(left) + Number(right);
			case '===':
				return left === right;
			case '!==':
				return left !== right;
			case '==':
			case '!=': {
				const equal =
					typeof left === typeof right || left === null || right === null
						? left === right
						: Number(left) === Number(right);
				return operator === '==' ? equal : !equal;
			}
			default:
				return UNRESOLVED;
		}
	}

	private evaluateIdentifier(node: Identifier, scope: Scope, depth: number): StaticValue {
		const declarations = getIdentifierDeclarations(node);
		if (declarations.length !== 1) return UNRESOLVED;
		const declaration = declarations[0];
		const bound = scope.get(declaration);
		if (bound !== undefined) return bound;
		if (Node.isImportSpecifier(declaration)) {
			if (declaration.isTypeOnly() || declaration.getImportDeclaration().isTypeOnly()) return UNRESOLVED;
			return this.evaluateImportedConstant(node.getSourceFile(), node.getText(), depth);
		}
		return this.evaluateBinding(declaration, scope, depth);
	}

	private evaluateBinding(binding: Node, scope: Scope, depth: number): StaticValue {
		if (this.inFlight.has(binding)) {
			return UNRESOLVED;
		}
		this.inFlight.add(binding);
		try {
			if (Node.isVariableDeclaration(binding)) {
				const declarationList = binding.getParent();
				if (!Node.isVariableDeclarationList(declarationList) || declarationList.getDeclarationKind() !== 'const') {
					return UNRESOLVED;
				}
				const initializer = binding.getInitializer();
				return initializer == null ? UNRESOLVED : this.evaluate(initializer, scope, depth + 1);
			}
			if (!Node.isBindingElement(binding)) {
				return UNRESOLVED;
			}
			if (binding.getDotDotDotToken() != null) {
				return UNRESOLVED;
			}
			const pattern = binding.getParent();
			const declaration = pattern.getParent();
			if (!Node.isVariableDeclaration(declaration)) {
				return UNRESOLVED;
			}
			const declarationList = declaration.getParent();
			if (!Node.isVariableDeclarationList(declarationList) || declarationList.getDeclarationKind() !== 'const') {
				return UNRESOLVED;
			}
			const initializer = declaration.getInitializer();
			if (initializer == null) {
				return UNRESOLVED;
			}
			const source = this.evaluate(initializer, scope, depth + 1);
			if (source === UNRESOLVED) {
				return UNRESOLVED;
			}
			if (Node.isObjectBindingPattern(pattern)) {
				if (!isPlainObject(source)) {
					return UNRESOLVED;
				}
				const key = this.evaluatePropertyName(binding.getPropertyNameNode() ?? binding.getNameNode(), scope, depth + 1);
				return key !== UNRESOLVED && Object.hasOwn(source, key) ? source[key] : UNRESOLVED;
			}
			if (Node.isArrayBindingPattern(pattern)) {
				if (!Array.isArray(source)) {
					return UNRESOLVED;
				}
				const index = pattern.getElements().indexOf(binding);
				return index >= 0 && index < source.length ? source[index] : UNRESOLVED;
			}
			return UNRESOLVED;
		} finally {
			this.inFlight.delete(binding);
		}
	}

	private evaluateImportedConstant(sourceFile: SourceFile, name: string, depth: number): StaticValue {
		const target = this.resolveImportTarget(sourceFile, name);
		if (target == null) {
			return UNRESOLVED;
		}
		const exported = target.sourceFile.getExportSymbols().find((symbol) => symbol.getName() === target.exportedName);
		const declarations = (exported?.getAliasedSymbol() ?? exported)?.getDeclarations();
		if (declarations?.length !== 1) return UNRESOLVED;
		return this.evaluateBinding(declarations[0], EMPTY_SCOPE, depth + 1);
	}

	private resolveImportTarget(
		sourceFile: SourceFile,
		name: string,
	): {sourceFile: SourceFile; exportedName: string} | null {
		for (const declaration of sourceFile.getImportDeclarations()) {
			for (const named of declaration.getNamedImports()) {
				const localName = named.getAliasNode()?.getText() ?? named.getName();
				if (localName !== name) {
					continue;
				}
				const resolved = this.resolveModule(sourceFile, declaration.getModuleSpecifierValue());
				if (resolved == null) {
					return null;
				}
				return {sourceFile: resolved, exportedName: named.getName()};
			}
		}
		return null;
	}

	private resolveSpecifierBase(fromPath: string, specifier: string): string | null {
		if (specifier.startsWith('.')) {
			return path.resolve(path.dirname(fromPath), specifier);
		}
		if (!specifier.startsWith(APP_ALIAS_PREFIX)) {
			return null;
		}
		const sourceRoot = this.findAppSourceRoot(path.dirname(fromPath));
		return sourceRoot == null ? null : path.join(sourceRoot, specifier.slice(APP_ALIAS_PREFIX.length));
	}

	private findAppSourceRoot(directory: string): string | null {
		const cached = this.appSourceRoots.get(directory);
		if (cached !== undefined) {
			return cached;
		}
		const fileSystem = this.project.getFileSystem();
		const tsconfigPath = path.join(directory, 'tsconfig.json');
		const parent = path.dirname(directory);
		let sourceRoot: string | null = null;
		if (fileSystem.fileExistsSync(tsconfigPath)) {
			const {config} = ts.readConfigFile(tsconfigPath, (filePath) => fileSystem.readFileSync(filePath));
			const target = config?.compilerOptions?.paths?.[`${APP_ALIAS_PREFIX}*`]?.[0];
			if (typeof target === 'string' && target.endsWith('/*')) {
				sourceRoot = path.resolve(directory, target.slice(0, -2));
			}
		} else if (parent !== directory) {
			sourceRoot = this.findAppSourceRoot(parent);
		}
		this.appSourceRoots.set(directory, sourceRoot);
		return sourceRoot;
	}

	private resolveModule(from: SourceFile, specifier: string): SourceFile | null {
		const base = this.resolveSpecifierBase(from.getFilePath(), specifier);
		if (base == null) {
			return null;
		}
		for (const candidate of [`${base}.ts`, `${base}.tsx`, `${base}/index.ts`, base]) {
			const existing = this.project.getSourceFile(candidate);
			if (existing != null) {
				return existing;
			}
			const added = this.project.addSourceFileAtPathIfExists(candidate);
			if (added != null) {
				return added;
			}
		}
		return null;
	}
}
