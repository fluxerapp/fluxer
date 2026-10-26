// SPDX-License-Identifier: AGPL-3.0-or-later

import {CLOSE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {EditorState, type Extension} from '@codemirror/state';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const REPLACE_DESCRIPTOR = msg({
	context: 'code-editor',
	message: 'Replace',
	comment: 'Text in the code editor of the quick CSS editor. Placeholder of the replace field and its button.',
});

const PHRASES: Readonly<Record<string, MessageDescriptor>> = {
	Find: msg({
		context: 'code-editor',
		message: 'Find',
		comment: 'Text in the code editor of the quick CSS editor. Placeholder of the search field.',
	}),
	Replace: REPLACE_DESCRIPTOR,
	replace: REPLACE_DESCRIPTOR,
	next: msg({
		context: 'code-editor',
		message: 'Next',
		comment: 'Text in the code editor of the quick CSS editor. Button that goes to the next search match.',
	}),
	previous: msg({
		context: 'code-editor',
		message: 'Previous',
		comment: 'Text in the code editor of the quick CSS editor. Button that goes to the previous search match.',
	}),
	all: msg({
		context: 'code-editor',
		message: 'All',
		comment: 'Text in the code editor of the quick CSS editor. Button that selects every search match.',
	}),
	'match case': msg({
		context: 'code-editor',
		message: 'Match case',
		comment: 'Text in the code editor of the quick CSS editor. Search option.',
	}),
	regexp: msg({
		context: 'code-editor',
		message: 'Regular expression',
		comment: 'Text in the code editor of the quick CSS editor. Search option.',
	}),
	'by word': msg({
		context: 'code-editor',
		message: 'Whole word',
		comment: 'Text in the code editor of the quick CSS editor. Search option.',
	}),
	'replace all': msg({
		context: 'code-editor',
		message: 'Replace all',
		comment: 'Text in the code editor of the quick CSS editor. Button that replaces every search match.',
	}),
	close: CLOSE_DESCRIPTOR,
	'current match': msg({
		context: 'code-editor',
		message: 'Current match',
		comment: 'Announced by screen readers in the code editor of the quick CSS editor.',
	}),
	'on line': msg({
		context: 'code-editor',
		message: 'on line',
		comment: 'Announced by screen readers in the code editor of the quick CSS editor. Followed by a line number.',
	}),
	'replaced $ matches': msg({
		context: 'code-editor',
		message: 'Replaced $ matches',
		comment:
			'Announced by screen readers in the code editor of the quick CSS editor. Keep the $ sign, it is replaced with a number.',
	}),
	'replaced match on line $': msg({
		context: 'code-editor',
		message: 'Replaced the match on line $',
		comment:
			'Announced by screen readers in the code editor of the quick CSS editor. Keep the $ sign, it is replaced with a line number.',
	}),
	'Go to line': msg({
		context: 'code-editor',
		message: 'Go to line',
		comment: 'Text in the code editor of the quick CSS editor. Label of the field that moves to a line number.',
	}),
	go: msg({
		context: 'code-editor',
		message: 'Go',
		comment:
			'Text in the code editor of the quick CSS editor. Button that moves to the line number typed in "Go to line".',
	}),
	'Fold line': msg({
		context: 'code-editor',
		message: 'Fold line',
		comment: 'Text in the code editor of the quick CSS editor. Tooltip of the arrow that hides a block of code.',
	}),
	'Unfold line': msg({
		context: 'code-editor',
		message: 'Unfold line',
		comment: 'Text in the code editor of the quick CSS editor. Tooltip of the arrow that shows a hidden block of code.',
	}),
	'folded code': msg({
		context: 'code-editor',
		message: 'Folded code',
		comment: 'Text in the code editor of the quick CSS editor. Label of a hidden block of code.',
	}),
	unfold: msg({
		context: 'code-editor',
		message: 'Unfold',
		comment: 'Text in the code editor of the quick CSS editor. Label of the button that shows a hidden block of code.',
	}),
	'Folded lines': msg({
		context: 'code-editor',
		message: 'Folded lines',
		comment: 'Announced by screen readers in the code editor of the quick CSS editor.',
	}),
	'Unfolded lines': msg({
		context: 'code-editor',
		message: 'Unfolded lines',
		comment: 'Announced by screen readers in the code editor of the quick CSS editor.',
	}),
	to: msg({
		context: 'code-editor',
		message: 'to',
		comment:
			'Announced by screen readers in the code editor of the quick CSS editor, between two line numbers, as in "Folded lines 3 to 7".',
	}),
	'Control character': msg({
		context: 'code-editor',
		message: 'Control character',
		comment:
			'Text in the code editor of the quick CSS editor. Tooltip on an invisible control character shown as a placeholder.',
	}),
	Diagnostics: msg({
		context: 'code-editor',
		message: 'Problems',
		comment: 'Text in the code editor of the quick CSS editor. Title of the list of problems in the CSS.',
	}),
	'No diagnostics': msg({
		context: 'code-editor',
		message: 'No problems.',
		comment: 'Text in the code editor of the quick CSS editor. Shown when the CSS has no problems.',
	}),
	Completions: msg({
		context: 'code-editor',
		message: 'Completions',
		comment: 'Announced by screen readers in the code editor of the quick CSS editor. Name of the list of suggestions.',
	}),
	'Selection deleted': msg({
		context: 'code-editor',
		message: 'Selection deleted',
		comment: 'Announced by screen readers in the code editor of the quick CSS editor.',
	}),
};

export function editorPhrases(i18n: I18n): Extension {
	return EditorState.phrases.of(
		Object.fromEntries(Object.entries(PHRASES).map(([phrase, descriptor]) => [phrase, i18n._(descriptor)])),
	);
}
