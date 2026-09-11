// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {registerComposerMarkdownHighlight} from '@app/features/lexical/composer/ComposerMarkdownHighlight';
import {$hydrateComposerFromDraft, $projectComposer} from '@app/features/lexical/composer/ComposerSerialization';
import {$getComposerSelectionRange, $selectComposerRange} from '@app/features/lexical/composer/composerOffsets';
import {
	computeMarkdownHighlightSpans,
	DEFAULT_COMPOSER_MARKDOWN_FLAGS,
	MarkdownHl,
	markSilentMessagePrefix,
} from '@app/features/lexical/composer/markdownSpans';
import {
	$isComposerMentionNode,
	ComposerMentionNode,
	ComposerMentionPresentation,
} from '@app/features/lexical/composer/nodes/ComposerMentionNode';
import {SyntaxMarkerNode} from '@app/features/lexical/composer/nodes/SyntaxMarkerNode';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import {
	$createParagraphNode,
	$createTextNode,
	$getRoot,
	$getSelection,
	$isElementNode,
	$isTextNode,
	createEditor,
	type LexicalEditor,
	type RangeSelection,
} from 'lexical';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/lexical/composer/nodes/ComposerMentionPill', () => ({ComposerMentionPill: () => null}));
vi.mock('@app/features/lexical/composer/nodes/ComposerCustomEmoji', () => ({ComposerCustomEmoji: () => null}));
vi.mock('@app/features/lexical/composer/nodes/ComposerStandardEmoji', () => ({ComposerStandardEmoji: () => null}));

const SILENT_STYLE = 'color:var(--markup-mention-text);font-weight:500';

function createComposer(silentMessagePrefix: boolean): {editor: LexicalEditor; unregister: () => void} {
	const editor = createEditor({
		namespace: 'test',
		nodes: [ComposerMentionNode, SyntaxMarkerNode],
		onError: (error) => {
			throw error;
		},
	});
	const unregister = registerComposerMarkdownHighlight(editor, DEFAULT_COMPOSER_MARKDOWN_FLAGS, silentMessagePrefix);
	return {editor, unregister};
}

function hydrate(editor: LexicalEditor, display: string, segments: Array<MentionSegment> = []): void {
	editor.update(() => $hydrateComposerFromDraft(display, segments), {discrete: true});
}

function edit(editor: LexicalEditor, start: number, end: number, text: string): void {
	editor.update(
		() => {
			$selectComposerRange(start, end);
			($getSelection() as RangeSelection).insertText(text);
		},
		{discrete: true},
	);
}

function readLines(editor: LexicalEditor): Array<Array<[string, string] | string>> {
	return editor.getEditorState().read(
		() =>
			$getRoot()
				.getChildren()
				.map((block) =>
					$isElementNode(block)
						? block
								.getChildren()
								.map((node) => ($isTextNode(node) ? [node.getTextContent(), node.getStyle()] : node.getType()))
						: [],
				),
		{editor},
	);
}

function readSelection(editor: LexicalEditor): {start: number; end: number} | null {
	return editor.getEditorState().read(() => $getComposerSelectionRange(), {editor});
}

describe('markSilentMessagePrefix', () => {
	it('marks @silent at the start of the message', () => {
		const source = '@silent hello';
		expect(markSilentMessagePrefix(computeMarkdownHighlightSpans(source), source)).toEqual([
			{start: 0, end: 7, role: 'content', format: MarkdownHl.silent},
			{start: 7, end: 13, role: 'content', format: MarkdownHl.none},
		]);
	});

	it('marks @silent on its own', () => {
		const source = '@silent';
		expect(markSilentMessagePrefix(computeMarkdownHighlightSpans(source), source)).toEqual([
			{start: 0, end: 7, role: 'content', format: MarkdownHl.silent},
		]);
	});

	it('marks @silent after leading whitespace', () => {
		const source = '  @silent hi';
		expect(markSilentMessagePrefix(computeMarkdownHighlightSpans(source), source)).toEqual([
			{start: 0, end: 2, role: 'content', format: MarkdownHl.none},
			{start: 2, end: 9, role: 'content', format: MarkdownHl.silent},
			{start: 9, end: 12, role: 'content', format: MarkdownHl.none},
		]);
	});

	it('marks @silent before a line break', () => {
		const source = '@silent\nhello';
		expect(markSilentMessagePrefix(computeMarkdownHighlightSpans(source), source)).toEqual([
			{start: 0, end: 7, role: 'content', format: MarkdownHl.silent},
			{start: 7, end: 13, role: 'content', format: MarkdownHl.none},
		]);
	});

	it('keeps markdown formats after @silent', () => {
		const source = '@silent **hi**';
		expect(markSilentMessagePrefix(computeMarkdownHighlightSpans(source), source)).toEqual([
			{start: 0, end: 7, role: 'content', format: MarkdownHl.silent},
			{start: 7, end: 8, role: 'content', format: MarkdownHl.none},
			{start: 8, end: 10, role: 'marker', format: MarkdownHl.bold},
			{start: 10, end: 12, role: 'content', format: MarkdownHl.bold},
			{start: 12, end: 14, role: 'marker', format: MarkdownHl.bold},
		]);
	});

	it('adds the silent bit to every span the token overlaps', () => {
		expect(
			markSilentMessagePrefix(
				[
					{start: 0, end: 3, role: 'content', format: MarkdownHl.none},
					{start: 3, end: 10, role: 'content', format: MarkdownHl.bold},
				],
				'@silent hi',
			),
		).toEqual([
			{start: 0, end: 3, role: 'content', format: MarkdownHl.silent},
			{start: 3, end: 7, role: 'content', format: MarkdownHl.bold | MarkdownHl.silent},
			{start: 7, end: 10, role: 'content', format: MarkdownHl.bold},
		]);
	});

	it.each(['@silently hello', '@Silent hello', 'hello @silent', ''])('returns the spans unchanged for %j', (source) => {
		const spans = computeMarkdownHighlightSpans(source);
		expect(markSilentMessagePrefix(spans, source)).toBe(spans);
	});
});

describe('registerComposerMarkdownHighlight with silentMessagePrefix', () => {
	it('styles @silent at the start of the message without changing what is sent', () => {
		const {editor} = createComposer(true);
		hydrate(editor, '@silent hello');
		expect(readLines(editor)).toEqual([
			[
				['@silent', SILENT_STYLE],
				[' hello', ''],
			],
		]);
		expect(editor.getEditorState().read(() => $projectComposer(), {editor})).toEqual({
			display: '@silent hello',
			segments: [],
			wire: '@silent hello',
		});
	});

	it('styles @silent on its own', () => {
		const {editor} = createComposer(true);
		hydrate(editor, '@silent');
		expect(readLines(editor)).toEqual([[['@silent', SILENT_STYLE]]]);
	});

	it('styles @silent before a line break', () => {
		const {editor} = createComposer(true);
		hydrate(editor, '@silent\nhello');
		expect(readLines(editor)).toEqual([[['@silent', SILENT_STYLE], 'linebreak', ['hello', '']]]);
	});

	it('styles @silent after leading whitespace', () => {
		const {editor} = createComposer(true);
		hydrate(editor, '  @silent hello');
		expect(readLines(editor)).toEqual([
			[
				['  ', ''],
				['@silent', SILENT_STYLE],
				[' hello', ''],
			],
		]);
	});

	it('styles @silent after a leading line break', () => {
		const {editor} = createComposer(true);
		hydrate(editor, '\n@silent hello');
		expect(readLines(editor)).toEqual([['linebreak', ['@silent', SILENT_STYLE], [' hello', '']]]);
	});

	it('leaves @silent unstyled when the prefix is off', () => {
		const {editor} = createComposer(false);
		hydrate(editor, '@silent hello');
		expect(readLines(editor)).toEqual([[['@silent hello', '']]]);
	});

	it('leaves @silent unstyled when it is not at the start', () => {
		const {editor} = createComposer(true);
		hydrate(editor, 'hello @silent');
		expect(readLines(editor)).toEqual([[['hello @silent', '']]]);
	});

	it('styles only the first paragraph', () => {
		const {editor} = createComposer(true);
		editor.update(
			() => {
				$getRoot()
					.clear()
					.append(
						$createParagraphNode().append($createTextNode('@silent hi')),
						$createParagraphNode().append($createTextNode('@silent hi')),
					);
			},
			{discrete: true},
		);
		expect(readLines(editor)).toEqual([
			[
				['@silent', SILENT_STYLE],
				[' hi', ''],
			],
			[['@silent hi', '']],
		]);
	});

	it('restyles the token as it is typed, broken and repaired', () => {
		const {editor} = createComposer(true);
		hydrate(editor, '@silen');
		edit(editor, 6, 6, 't');
		expect(readLines(editor)).toEqual([[['@silent', SILENT_STYLE]]]);
		expect(readSelection(editor)).toEqual({start: 7, end: 7});
		edit(editor, 7, 7, 'l');
		expect(readLines(editor)).toEqual([[['@silentl', '']]]);
		expect(readSelection(editor)).toEqual({start: 8, end: 8});
		edit(editor, 7, 8, '');
		expect(readLines(editor)).toEqual([[['@silent', SILENT_STYLE]]]);
		expect(readSelection(editor)).toEqual({start: 7, end: 7});
	});

	it('styles @silent typed in front of existing text', () => {
		const {editor} = createComposer(true);
		hydrate(editor, 'hello');
		edit(editor, 0, 0, '@silent ');
		expect(readLines(editor)).toEqual([
			[
				['@silent', SILENT_STYLE],
				[' hello', ''],
			],
		]);
		expect(readSelection(editor)).toEqual({start: 8, end: 8});
	});

	it('keeps an autocompleted @everyone mention after @silent', () => {
		const segment: MentionSegment = {
			type: 'special',
			id: '@everyone',
			displayText: '@everyone',
			actualText: '@everyone',
			start: 8,
			end: 17,
		};
		const {editor} = createComposer(true);
		hydrate(editor, '@silent @everyone', [segment]);
		expect(readLines(editor)).toEqual([[['@silent', SILENT_STYLE], [' ', ''], 'composer-mention']]);
		expect(editor.getEditorState().read(() => $projectComposer(), {editor})).toEqual({
			display: '@silent @everyone',
			segments: [segment],
			wire: '@silent @everyone',
		});
	});

	it('keeps a user mention after @silent as a plain pill', () => {
		const {editor} = createComposer(true);
		hydrate(editor, '@silent @name', [
			{type: 'user', id: '123', displayText: '@name', actualText: '<@123>', start: 8, end: 13},
		]);
		expect(readLines(editor)).toEqual([[['@silent', SILENT_STYLE], [' ', ''], 'composer-mention']]);
		const mention = editor.getEditorState().read(() => {
			const paragraph = $getRoot().getFirstChildOrThrow();
			assert($isElementNode(paragraph), 'Expected composer paragraph');
			const pill = paragraph.getLastChild();
			return $isComposerMentionNode(pill) ? {literal: pill.isLiteral(), presentation: pill.getPresentation()} : null;
		});
		expect(mention).toEqual({literal: false, presentation: ComposerMentionPresentation.none});
	});

	it('removes the style when the prefix is switched off', () => {
		const {editor, unregister} = createComposer(true);
		hydrate(editor, '@silent hello');
		unregister();
		registerComposerMarkdownHighlight(editor, DEFAULT_COMPOSER_MARKDOWN_FLAGS, false);
		editor.update(
			() => {
				for (const node of $getRoot().getAllTextNodes()) {
					node.markDirty();
				}
			},
			{discrete: true},
		);
		expect(readLines(editor)).toEqual([[['@silent hello', '']]]);
	});
});
