// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CommandOption} from '@app/features/devtools/hooks/useCommands';
import {parsePersistedSlashCommandOptions} from '@app/features/lexical/composer/SlashSlotPersistence';
import {
	type EditorConfig,
	type LexicalNode,
	type NodeKey,
	type SerializedTextNode,
	type Spread,
	TextNode,
} from 'lexical';

export type SerializedComposerCommandNode = Spread<
	{
		optionalOptions: ReadonlyArray<CommandOption>;
	},
	SerializedTextNode
>;

export class ComposerCommandNode extends TextNode {
	__optionalOptions: ReadonlyArray<CommandOption>;

	static override getType(): string {
		return 'composer-command';
	}

	static override clone(node: ComposerCommandNode): ComposerCommandNode {
		return new ComposerCommandNode(node.__text, node.__optionalOptions, node.__key);
	}

	static override importJSON(serializedNode: SerializedComposerCommandNode): ComposerCommandNode {
		const parsedOptions = parsePersistedSlashCommandOptions(serializedNode.optionalOptions);
		const optionalOptions = parsedOptions == null ? [] : parsedOptions;
		return $createComposerCommandNode(serializedNode.text, optionalOptions).updateFromJSON(serializedNode);
	}

	constructor(text: string, optionalOptions: ReadonlyArray<CommandOption> = [], key?: NodeKey) {
		super(text, key);
		this.__optionalOptions = optionalOptions;
	}

	override exportJSON(): SerializedComposerCommandNode {
		return {
			...super.exportJSON(),
			optionalOptions: this.__optionalOptions,
		};
	}

	getOptionalOptions(): ReadonlyArray<CommandOption> {
		return this.getLatest().__optionalOptions;
	}

	private applyCommandClass(dom: HTMLElement, config: EditorConfig): void {
		const className = config.theme.composerCommand;
		if (typeof className === 'string' && className.length > 0) {
			dom.classList.add(className);
		}
	}

	override createDOM(config: EditorConfig): HTMLElement {
		const dom = super.createDOM(config);
		this.applyCommandClass(dom, config);
		return dom;
	}

	override canInsertTextBefore(): false {
		return false;
	}

	override canInsertTextAfter(): false {
		return false;
	}
}

export function $createComposerCommandNode(
	text: string,
	optionalOptions: ReadonlyArray<CommandOption> = [],
): ComposerCommandNode {
	return new ComposerCommandNode(text, optionalOptions);
}

export function $isComposerCommandNode(node: LexicalNode | null | undefined): node is ComposerCommandNode {
	return node instanceof ComposerCommandNode;
}
