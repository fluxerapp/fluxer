// SPDX-License-Identifier: AGPL-3.0-or-later

import {registerComposerCodeIndent} from '@app/features/lexical/composer/ComposerCodeIndent';
import {
	$getComposerDisplayText,
	$replaceComposerRange,
	$selectComposerRange,
} from '@app/features/lexical/composer/composerOffsets';
import {ComposerMentionNode} from '@app/features/lexical/composer/nodes/ComposerMentionNode';
import {createEditor, KEY_TAB_COMMAND} from 'lexical';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/lexical/composer/nodes/ComposerMentionPill', () => ({ComposerMentionPill: () => null}));
vi.mock('@app/features/lexical/composer/nodes/ComposerCustomEmoji', () => ({ComposerCustomEmoji: () => null}));
vi.mock('@app/features/lexical/composer/nodes/ComposerStandardEmoji', () => ({ComposerStandardEmoji: () => null}));

describe('registerComposerCodeIndent', () => {
	it('indents a block opened after a mention whose name holds a backtick', () => {
		const editor = createEditor({
			namespace: 'composer-code-indent-test',
			nodes: [ComposerMentionNode],
			onError: (error) => {
				throw error;
			},
		});
		registerComposerCodeIndent(editor, {current: false});
		editor.update(
			() => {
				$replaceComposerRange(
					0,
					0,
					{kind: 'mention', mentionType: 'user', id: '1', display: '@`bob', wire: '<@1>'},
					{trailing: false},
				);
				$replaceComposerRange(5, 5, {kind: 'text', text: ' hi ```js\ncode'}, {leading: false, trailing: false});
				$selectComposerRange(19, 19);
			},
			{discrete: true},
		);
		const event = Object.assign(new Event('keydown', {cancelable: true}), {shiftKey: false}) as KeyboardEvent;
		expect(editor.dispatchCommand(KEY_TAB_COMMAND, event)).toBe(true);
		expect(editor.read(() => $getComposerDisplayText())).toBe('@`bob hi ```js\ncode\t');
	});
});
