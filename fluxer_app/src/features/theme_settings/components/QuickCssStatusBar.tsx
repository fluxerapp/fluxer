// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/theme_settings/components/QuickCssEditor.module.css';
import {QuickCssShortcutsModal} from '@app/features/theme_settings/components/QuickCssShortcutsModal';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {openLintPanel} from '@codemirror/lint';
import {gotoLine} from '@codemirror/search';
import type {EditorView} from '@codemirror/view';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {KeyboardIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useSyncExternalStore} from 'react';

const LINE_COLUMN_DESCRIPTOR = msg({
	message: 'Line {line}, column {column}',
	comment: 'Button in the status bar of the quick CSS editor. Selecting it moves to another line.',
});
const DOCUMENT_SIZE_DESCRIPTOR = msg({
	message:
		'{lines, plural, one {# line} other {# lines}} and {characters, plural, one {# character} other {# characters}}',
	comment: 'Tooltip of the line and column button in the status bar of the quick CSS editor.',
});
const SELECTED_DESCRIPTOR = msg({
	message: '{count, plural, one {# character selected} other {# characters selected}}',
	comment: 'Shown in the status bar of the quick CSS editor while text is selected.',
});
const ERRORS_DESCRIPTOR = msg({
	message: '{count, plural, one {# error} other {# errors}}',
	comment: 'Button in the status bar of the quick CSS editor that opens the list of problems.',
});
const WARNINGS_DESCRIPTOR = msg({
	message: '{count, plural, one {# warning} other {# warnings}}',
	comment: 'Button in the status bar of the quick CSS editor that opens the list of problems.',
});
const KEYBOARD_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Keyboard shortcuts',
	comment: 'Button in the status bar of the quick CSS editor that lists its keyboard shortcuts.',
});

export interface EditorStatus {
	line: number;
	column: number;
	selected: number;
	lines: number;
	characters: number;
	errors: number;
	warnings: number;
	revision: number;
}

export interface EditorStatusStore {
	get: () => EditorStatus;
	set: (status: EditorStatus) => void;
	subscribe: (listener: () => void) => () => void;
}

const EMPTY_STATUS: EditorStatus = {
	line: 1,
	column: 1,
	selected: 0,
	lines: 1,
	characters: 0,
	errors: 0,
	warnings: 0,
	revision: 0,
};

function sameStatus(left: EditorStatus, right: EditorStatus): boolean {
	return (Object.keys(left) as Array<keyof EditorStatus>).every((key) => left[key] === right[key]);
}

export function createEditorStatusStore(): EditorStatusStore {
	let current = EMPTY_STATUS;
	const listeners = new Set<() => void>();
	return {
		get: () => current,
		set: (next) => {
			if (sameStatus(current, next)) return;
			current = next;
			for (const listener of listeners) listener();
		},
		subscribe: (listener) => {
			listeners.add(listener);
			return () => listeners.delete(listener);
		},
	};
}

interface QuickCssStatusBarProps {
	view: EditorView | null;
	status: EditorStatusStore;
	previewStatus: string | null;
}

export const QuickCssStatusBar: React.FC<QuickCssStatusBarProps> = ({view, status, previewStatus}) => {
	const {i18n} = useLingui();
	const current = useSyncExternalStore(status.subscribe, status.get);
	const openProblems = () => {
		if (view) openLintPanel(view);
	};
	return (
		<div className={styles.statusBar} data-flx="theme-settings.quick-css-status-bar.root">
			<Tooltip
				text={i18n._(DOCUMENT_SIZE_DESCRIPTOR, {lines: current.lines, characters: current.characters})}
				position="top"
				data-flx="theme-settings.quick-css-status-bar.position-tooltip"
			>
				<button
					type="button"
					className={styles.statusButton}
					onClick={() => {
						if (view) gotoLine(view);
					}}
					data-flx="theme-settings.quick-css-status-bar.position"
				>
					{i18n._(LINE_COLUMN_DESCRIPTOR, {line: current.line, column: current.column})}
				</button>
			</Tooltip>
			{current.selected > 0 ? (
				<span className={styles.statusText} data-flx="theme-settings.quick-css-status-bar.selected">
					{i18n._(SELECTED_DESCRIPTOR, {count: current.selected})}
				</span>
			) : null}
			<button
				type="button"
				className={styles.statusButton}
				data-severity={current.errors > 0 ? 'error' : undefined}
				onClick={openProblems}
				data-flx="theme-settings.quick-css-status-bar.errors"
			>
				{i18n._(ERRORS_DESCRIPTOR, {count: current.errors})}
			</button>
			<button
				type="button"
				className={styles.statusButton}
				data-severity={current.warnings > 0 ? 'warning' : undefined}
				onClick={openProblems}
				data-flx="theme-settings.quick-css-status-bar.warnings"
			>
				{i18n._(WARNINGS_DESCRIPTOR, {count: current.warnings})}
			</button>
			<span
				className={styles.statusPreview}
				role="status"
				title={previewStatus ?? undefined}
				data-flx="theme-settings.quick-css-status-bar.preview"
			>
				{previewStatus}
			</span>
			<Tooltip
				text={i18n._(KEYBOARD_SHORTCUTS_DESCRIPTOR)}
				position="top"
				data-flx="theme-settings.quick-css-status-bar.shortcuts-tooltip"
			>
				<button
					type="button"
					className={clsx(styles.statusButton, styles.statusIconButton)}
					aria-label={i18n._(KEYBOARD_SHORTCUTS_DESCRIPTOR)}
					onClick={() =>
						ModalCommands.push(
							modal(() => (
								<QuickCssShortcutsModal data-flx="theme-settings.quick-css-status-bar.quick-css-shortcuts-modal" />
							)),
						)
					}
					data-flx="theme-settings.quick-css-status-bar.shortcuts"
				>
					<KeyboardIcon size={remFromPx(18)} data-flx="theme-settings.quick-css-status-bar.keyboard-icon" />
				</button>
			</Tooltip>
		</div>
	);
};
