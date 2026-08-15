// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	$createSlashSlotPlaceholderNode,
	$isSlashSlotPlaceholderNode,
	type SlashSlotPlaceholderNode,
} from '@app/features/lexical/composer/nodes/SlashSlotPlaceholderNode';
import type {SlashSlotType, SlashSlotValidationError} from '@app/features/lexical/composer/slashSlotValidation';
import {
	$applyNodeReplacement,
	$isTextNode,
	type EditorConfig,
	ElementNode,
	type LexicalNode,
	type NodeKey,
	type SerializedElementNode,
	type Spread,
} from 'lexical';

export type SlashSlotValidity = 'neutral' | 'valid' | 'invalid';

export type SerializedSlashSlotNode = Spread<
	{
		optionName: string;
		optionType: SlashSlotType;
		required: boolean;
		allowEmpty?: boolean;
		description: string;
		choices: ReadonlyArray<{name: string; value: string}>;
		validity: SlashSlotValidity;
		validationError: SlashSlotValidationError | null;
		touched: boolean;
		resolvedWire: string | null;
		resolvedDisplay: string | null;
	},
	SerializedElementNode
>;

export class SlashSlotNode extends ElementNode {
	__optionName: string;
	__optionType: SlashSlotType;
	__required: boolean;
	__description: string;
	__choices: ReadonlyArray<{name: string; value: string}>;
	__validity: SlashSlotValidity;
	__validationError: SlashSlotValidationError | null;
	__touched: boolean;
	__resolvedWire: string | null;
	__resolvedDisplay: string | null;
	__allowEmpty: boolean;

	static override getType(): string {
		return 'slash-slot';
	}

	static override clone(node: SlashSlotNode): SlashSlotNode {
		const cloned = new SlashSlotNode(
			node.__optionName,
			node.__optionType,
			node.__required,
			node.__choices,
			node.__description,
			node.__validity,
			node.__validationError,
			node.__touched,
			node.__resolvedWire,
			node.__resolvedDisplay,
			node.__key,
		);
		cloned.__allowEmpty = node.__allowEmpty;
		return cloned;
	}

	static override importJSON(serializedNode: SerializedSlashSlotNode): SlashSlotNode {
		return $createSlashSlotNode(
			serializedNode.optionName,
			serializedNode.optionType,
			serializedNode.required,
			serializedNode.choices == null ? [] : serializedNode.choices,
			serializedNode.description == null ? '' : serializedNode.description,
		)
			.updateFromJSON(serializedNode)
			.setAllowEmpty(serializedNode.allowEmpty === true)
			.setValidity(serializedNode.validity == null ? 'neutral' : serializedNode.validity)
			.setValidationError(serializedNode.validationError == null ? null : serializedNode.validationError)
			.setTouched(serializedNode.touched == null ? false : serializedNode.touched)
			.setResolvedWire(
				typeof serializedNode.resolvedWire === 'string' ? serializedNode.resolvedWire : null,
				serializedNode.resolvedDisplay == null ? null : serializedNode.resolvedDisplay,
			);
	}

	constructor(
		optionName: string,
		optionType: SlashSlotType,
		required: boolean,
		choices: ReadonlyArray<{name: string; value: string}> = [],
		description = '',
		validity: SlashSlotValidity = 'neutral',
		validationError: SlashSlotValidationError | null = null,
		touched = false,
		resolvedWire: string | null = null,
		resolvedDisplay: string | null = null,
		key?: NodeKey,
	) {
		super(key);
		this.__optionName = optionName;
		this.__optionType = optionType;
		this.__required = required;
		this.__description = description;
		this.__choices = choices;
		this.__validity = validity;
		this.__validationError = validationError;
		this.__touched = touched;
		this.__resolvedWire = resolvedWire;
		this.__resolvedDisplay = resolvedDisplay;
		this.__allowEmpty = false;
	}

	override exportJSON(): SerializedSlashSlotNode {
		return {
			...super.exportJSON(),
			optionName: this.__optionName,
			optionType: this.__optionType,
			required: this.__required,
			allowEmpty: this.__allowEmpty,
			description: this.__description,
			choices: this.__choices,
			validity: this.__validity,
			validationError: this.__validationError,
			touched: this.__touched,
			resolvedWire: this.__resolvedWire,
			resolvedDisplay: this.__resolvedDisplay,
		};
	}

	private applyDataset(element: HTMLElement): void {
		const latest = this.getLatest();
		const guidance = latest.getAccessibleDescription();
		element.setAttribute('data-slot-name', latest.__optionName);
		element.setAttribute('data-slot-validity', latest.__validity);
		element.setAttribute('data-slot-required', latest.__required ? 'true' : 'false');
		element.setAttribute('data-slot-type', latest.__optionType);
		element.setAttribute('data-slot-error', latest.__validationError == null ? '' : latest.__validationError);
		element.setAttribute('role', 'group');
		element.setAttribute('aria-label', latest.getAccessibleLabel());
		if (guidance.length > 0) {
			element.setAttribute('aria-description', guidance);
		} else {
			element.removeAttribute('aria-description');
		}
		element.setAttribute('aria-invalid', latest.__validity === 'invalid' ? 'true' : 'false');
	}

	override createDOM(config: EditorConfig): HTMLElement {
		const span = document.createElement('span');
		const className = config.theme.slashSlot;
		if (typeof className === 'string') {
			span.className = className;
		}
		span.spellcheck = false;
		span.setAttribute('data-lexical-composer-slot', 'true');
		this.applyDataset(span);
		return span;
	}

	override updateDOM(prevNode: SlashSlotNode, dom: HTMLElement): boolean {
		if (
			prevNode.__optionName !== this.__optionName ||
			prevNode.__optionType !== this.__optionType ||
			prevNode.__validity !== this.__validity ||
			prevNode.__validationError !== this.__validationError ||
			prevNode.__required !== this.__required ||
			prevNode.__description !== this.__description ||
			prevNode.__choices !== this.__choices
		) {
			this.applyDataset(dom);
		}
		return false;
	}

	getWireText(): string {
		const latest = this.getLatest();
		if (
			latest.__resolvedWire != null &&
			latest.__resolvedWire.length > 0 &&
			latest.__resolvedDisplay === this.getTextContent()
		) {
			return latest.__resolvedWire;
		}
		return this.getTextContent();
	}

	getOptionName(): string {
		return this.getLatest().__optionName;
	}

	getOptionType(): SlashSlotType {
		return this.getLatest().__optionType;
	}

	isRequired(): boolean {
		return this.getLatest().__required;
	}

	allowsEmpty(): boolean {
		return this.getLatest().__allowEmpty;
	}

	isSubmitRequired(): boolean {
		const latest = this.getLatest();
		return latest.__required && !latest.__allowEmpty;
	}

	getDescription(): string {
		return this.getLatest().__description;
	}

	getChoices(): ReadonlyArray<{name: string; value: string}> {
		return this.getLatest().__choices;
	}

	getValidity(): SlashSlotValidity {
		return this.getLatest().__validity;
	}

	getValidationError(): SlashSlotValidationError | null {
		return this.getLatest().__validationError;
	}

	isTouched(): boolean {
		return this.getLatest().__touched;
	}

	getResolvedWire(): string | null {
		return this.getLatest().__resolvedWire;
	}

	getResolvedDisplay(): string | null {
		return this.getLatest().__resolvedDisplay;
	}

	getAccessibleLabel(): string {
		const latest = this.getLatest();
		return `${latest.__optionName}, ${latest.__required ? 'required' : 'optional'} ${latest.__optionType} option`;
	}

	getAccessibleDescription(): string {
		const latest = this.getLatest();
		const parts: Array<string> = [];
		if (latest.__description.length > 0) {
			parts.push(latest.__description);
		}
		if (latest.__choices.length > 0) {
			parts.push(`Choices: ${latest.__choices.map((choice) => choice.name).join(', ')}`);
		}
		if (latest.__validity === 'invalid' && latest.__validationError != null) {
			parts.push(`Invalid: ${latest.__validationError}`);
		}
		return parts.join('. ');
	}

	isFilled(): boolean {
		return this.getTextContent().length > 0;
	}

	selectValueEnd(): void {
		const latest = this.getLatest();
		if (latest.getTextContentSize() > 0) {
			latest.selectEnd();
			return;
		}
		const placeholder = latest.ensurePlaceholder();
		placeholder.select(0, 0);
	}

	selectValueStart(): void {
		const latest = this.getLatest();
		const first = latest.getFirstChild();
		if ($isTextNode(first)) {
			first.select(0, 0);
			return;
		}
		latest.select(0, 0);
	}

	ensurePlaceholder(): SlashSlotPlaceholderNode {
		const writable = this.getWritable();
		let placeholder: SlashSlotPlaceholderNode | null = null;
		for (const child of writable.getChildren()) {
			if ($isSlashSlotPlaceholderNode(child) && placeholder == null) {
				placeholder = child;
			} else {
				child.remove();
			}
		}
		if (placeholder == null) {
			placeholder = $createSlashSlotPlaceholderNode();
			writable.append(placeholder);
		}
		return placeholder;
	}

	setValidity(validity: SlashSlotValidity): this {
		const writable = this.getWritable();
		writable.__validity = validity;
		if (validity !== 'neutral') {
			writable.__touched = true;
		}
		return writable;
	}

	setValidationError(validationError: SlashSlotValidationError | null): this {
		const writable = this.getWritable();
		writable.__validationError = validationError;
		return writable;
	}

	setTouched(touched: boolean): this {
		const writable = this.getWritable();
		writable.__touched = touched;
		return writable;
	}

	setAllowEmpty(allowEmpty: boolean): this {
		const writable = this.getWritable();
		writable.__allowEmpty = allowEmpty;
		return writable;
	}

	setResolvedWire(resolvedWire: string | null, resolvedDisplay?: string | null): this {
		const writable = this.getWritable();
		writable.__resolvedWire = resolvedWire;
		writable.__resolvedDisplay =
			resolvedWire == null ? null : resolvedDisplay == null ? this.getTextContent() : resolvedDisplay;
		if (resolvedWire != null) {
			writable.__validity = 'valid';
			writable.__validationError = null;
			writable.__touched = true;
		}
		return writable;
	}

	override isInline(): true {
		return true;
	}

	override canBeEmpty(): true {
		return true;
	}

	override canInsertTextBefore(): true {
		return true;
	}

	override canInsertTextAfter(): true {
		return true;
	}
}

export function $createSlashSlotNode(
	optionName: string,
	optionType: SlashSlotType,
	required: boolean,
	choices: ReadonlyArray<{name: string; value: string}> = [],
	description = '',
): SlashSlotNode {
	return $applyNodeReplacement(new SlashSlotNode(optionName, optionType, required, choices, description));
}

export function $isSlashSlotNode(node: LexicalNode | null | undefined): node is SlashSlotNode {
	return node instanceof SlashSlotNode;
}
