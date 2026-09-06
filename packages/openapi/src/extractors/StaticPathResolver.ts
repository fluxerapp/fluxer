// SPDX-License-Identifier: AGPL-3.0-or-later
import * as path from 'node:path';
import {Node, type Project, type SourceFile, SyntaxKind} from 'ts-morph';

export const UNRESOLVED = Symbol('unresolved');

export type StaticValue =
	| string
	| number
	| boolean
	| null
	| ReadonlyArray<StaticValue>
	| {readonly [key: string]: StaticValue};

type Resolved = StaticValue | typeof UNRESOLVED;

export type Scope = ReadonlyMap<string, StaticValue>;

export const EMPTY_SCOPE: Scope = new Map<string, StaticValue>();

const MAX_DEPTH = 48;

function isPlainObject(value: Resolved): value is {readonly [key: string]: StaticValue} {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function isTruthy(value: StaticValue): boolean {
	if (Array.isArray(value)) return true;
	if (isPlainObject(value)) return true;
	return Boolean(value);
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

export class StaticPathResolver {
	private readonly moduleConstants = new Map<SourceFile, Map<string, Node>>();
	private readonly inFlight = new Set<Node>();

	constructor(private readonly project: Project) {}

	public resolve(node: Node, scope: Scope): Resolved {
		return this.evaluate(node, scope, 0);
	}

	public resolveString(node: Node, scope: Scope): string | null {
		const value = this.evaluate(node, scope, 0);
		return typeof value === 'string' ? value : null;
	}

	private evaluate(rawNode: Node, scope: Scope, depth: number): Resolved {
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
				if (value === UNRESOLVED || Array.isArray(value) || isPlainObject(value)) {
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
			return name in target ? target[name] : UNRESOLVED;
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
				const index = typeof key === 'number' ? key : Number.parseInt(key, 10);
				return Number.isInteger(index) && index >= 0 && index < target.length ? target[index] : UNRESOLVED;
			}
			if (isPlainObject(target)) {
				const name = String(key);
				return name in target ? target[name] : UNRESOLVED;
			}
			return UNRESOLVED;
		}
		if (Node.isArrayLiteralExpression(node)) {
			const values: Array<StaticValue> = [];
			for (const element of node.getElements()) {
				if (Node.isSpreadElement(element)) {
					return UNRESOLVED;
				}
				const value = this.evaluate(element, scope, depth + 1);
				if (value === UNRESOLVED) {
					return UNRESOLVED;
				}
				values.push(value);
			}
			return values;
		}
		if (Node.isObjectLiteralExpression(node)) {
			const result: Record<string, StaticValue> = {};
			for (const property of node.getProperties()) {
				if (Node.isPropertyAssignment(property)) {
					const initializer = property.getInitializer();
					if (initializer == null) {
						return UNRESOLVED;
					}
					const value = this.evaluate(initializer, scope, depth + 1);
					if (value === UNRESOLVED) {
						continue;
					}
					result[property.getName()] = value;
					continue;
				}
				if (Node.isShorthandPropertyAssignment(property)) {
					const value = this.evaluate(property.getNameNode(), scope, depth + 1);
					if (value === UNRESOLVED) {
						continue;
					}
					result[property.getName()] = value;
					continue;
				}
				return UNRESOLVED;
			}
			return result;
		}
		if (Node.isConditionalExpression(node)) {
			const condition = this.evaluate(node.getCondition(), scope, depth + 1);
			if (condition === UNRESOLVED) {
				return UNRESOLVED;
			}
			return this.evaluate(isTruthy(condition) ? node.getWhenTrue() : node.getWhenFalse(), scope, depth + 1);
		}
		if (Node.isPrefixUnaryExpression(node)) {
			if (node.getOperatorToken() !== SyntaxKind.ExclamationToken) {
				return UNRESOLVED;
			}
			const operand = this.evaluate(node.getOperand(), scope, depth + 1);
			return operand === UNRESOLVED ? UNRESOLVED : !isTruthy(operand);
		}
		if (Node.isBinaryExpression(node)) {
			return this.evaluateBinary(node, scope, depth);
		}
		return UNRESOLVED;
	}

	private evaluateBinary(node: Node, scope: Scope, depth: number): Resolved {
		if (!Node.isBinaryExpression(node)) {
			return UNRESOLVED;
		}
		const operator = node.getOperatorToken().getText();
		const left = this.evaluate(node.getLeft(), scope, depth + 1);
		if (left === UNRESOLVED) {
			return UNRESOLVED;
		}
		if (operator === '&&') {
			return isTruthy(left) ? this.evaluate(node.getRight(), scope, depth + 1) : left;
		}
		if (operator === '||') {
			return isTruthy(left) ? left : this.evaluate(node.getRight(), scope, depth + 1);
		}
		if (operator === '??') {
			return left === null ? this.evaluate(node.getRight(), scope, depth + 1) : left;
		}
		const right = this.evaluate(node.getRight(), scope, depth + 1);
		if (right === UNRESOLVED) {
			return UNRESOLVED;
		}
		if (operator === '+') {
			if (typeof left === 'string' && (typeof right === 'string' || typeof right === 'number')) {
				return left + String(right);
			}
			if (typeof left === 'number' && typeof right === 'number') {
				return left + right;
			}
			return UNRESOLVED;
		}
		const comparable =
			(typeof left === 'string' || typeof left === 'number' || typeof left === 'boolean' || left === null) &&
			(typeof right === 'string' || typeof right === 'number' || typeof right === 'boolean' || right === null);
		if (!comparable) {
			return UNRESOLVED;
		}
		if (operator === '===' || operator === '==') {
			return left === right;
		}
		if (operator === '!==' || operator === '!=') {
			return left !== right;
		}
		return UNRESOLVED;
	}

	private evaluateIdentifier(node: Node, scope: Scope, depth: number): Resolved {
		if (!Node.isIdentifier(node)) {
			return UNRESOLVED;
		}
		const name = node.getText();
		if (name === 'undefined') {
			return UNRESOLVED;
		}
		const bound = scope.get(name);
		if (bound !== undefined) {
			return bound;
		}
		const local = this.findBindingInScopeChain(node, name);
		if (local != null) {
			return this.evaluateBinding(local, scope, depth);
		}
		return this.evaluateImportedConstant(node.getSourceFile(), name, depth);
	}

	private findBindingInScopeChain(from: Node, name: string): Node | null {
		for (const ancestor of from.getAncestors()) {
			if (!Node.isBlock(ancestor) && !Node.isSourceFile(ancestor) && !Node.isCaseClause(ancestor)) {
				continue;
			}
			for (const statement of ancestor.getStatements()) {
				if (!Node.isVariableStatement(statement)) {
					continue;
				}
				if (statement.getDeclarationKind() !== 'const') {
					continue;
				}
				for (const declaration of statement.getDeclarations()) {
					const nameNode = declaration.getNameNode();
					if (Node.isIdentifier(nameNode)) {
						if (nameNode.getText() === name) {
							return declaration;
						}
						continue;
					}
					if (Node.isObjectBindingPattern(nameNode) || Node.isArrayBindingPattern(nameNode)) {
						for (const element of nameNode.getElements()) {
							if (Node.isBindingElement(element) && element.getName() === name) {
								return element;
							}
						}
					}
				}
			}
		}
		return null;
	}

	private evaluateBinding(binding: Node, scope: Scope, depth: number): Resolved {
		if (this.inFlight.has(binding)) {
			return UNRESOLVED;
		}
		this.inFlight.add(binding);
		try {
			if (Node.isVariableDeclaration(binding)) {
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
				const key = binding.getPropertyNameNode()?.getText() ?? binding.getName();
				return key in source ? source[key] : UNRESOLVED;
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

	private evaluateImportedConstant(sourceFile: SourceFile, name: string, depth: number): Resolved {
		const target = this.resolveImportTarget(sourceFile, name);
		if (target == null) {
			return UNRESOLVED;
		}
		const constants = this.constantsOf(target.sourceFile);
		const initializer = constants.get(target.exportedName);
		if (initializer == null) {
			return UNRESOLVED;
		}
		return this.evaluate(initializer, EMPTY_SCOPE, depth + 1);
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

	private resolveModule(from: SourceFile, specifier: string): SourceFile | null {
		if (!specifier.startsWith('.')) {
			return null;
		}
		const base = path.resolve(path.dirname(from.getFilePath()), specifier);
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

	private constantsOf(sourceFile: SourceFile): Map<string, Node> {
		const cached = this.moduleConstants.get(sourceFile);
		if (cached != null) {
			return cached;
		}
		const constants = new Map<string, Node>();
		for (const statement of sourceFile.getVariableStatements()) {
			if (statement.getDeclarationKind() !== 'const') {
				continue;
			}
			for (const declaration of statement.getDeclarations()) {
				const nameNode = declaration.getNameNode();
				const initializer = declaration.getInitializer();
				if (Node.isIdentifier(nameNode) && initializer != null) {
					constants.set(nameNode.getText(), initializer);
				}
			}
		}
		this.moduleConstants.set(sourceFile, constants);
		return constants;
	}
}
