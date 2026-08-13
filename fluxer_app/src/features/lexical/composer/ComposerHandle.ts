// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CommandOption} from '@app/features/devtools/hooks/useCommands';
import type {ComposerInsertPayload, ComposerInsertSpacing} from '@app/features/lexical/composer/composerOffsets';
import type {
	SlashOptionalContext,
	SlashSlotAutocompleteContext,
	SlashSlotChoiceContext,
} from '@app/features/lexical/composer/slashSlots';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import type {LexicalEditor} from 'lexical';

export interface ComposerSelectionRange {
	start: number;
	end: number;
}

export interface ComposerHandle {
	focus: () => void;
	isFocused: () => boolean;
	getEditor: () => LexicalEditor;
	getDisplayValue: () => string;
	getWireValue: () => string;
	getSegments: () => Array<MentionSegment>;
	getTextUpToCursor: () => string;
	getSelection: () => ComposerSelectionRange | null;
	replaceRange: (start: number, end: number, payload: ComposerInsertPayload, spacing?: ComposerInsertSpacing) => void;
	insertSlashCommand: (name: string, options: ReadonlyArray<CommandOption>, start: number, end: number) => void;
	insertTextAtCursor: (text: string) => void;
	wrapSelection: (prefix: string, suffix: string) => void;
	deleteSelection: () => void;
	clear: () => void;
	getActiveSlotAutocompleteContext: () => SlashSlotAutocompleteContext | null;
	getActiveSlotChoiceContext: () => SlashSlotChoiceContext | null;
	applySlotChoice: (name: string) => void;
	applySlotPayload: (payload: ComposerInsertPayload) => void;
	getActiveOptionalContext: () => SlashOptionalContext | null;
	applyOptionalChoice: (name: string) => void;
	hydrate: (display: string, segments: ReadonlyArray<MentionSegment>) => void;
}
