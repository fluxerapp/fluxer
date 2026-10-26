// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import Keybind, {type KeyCombo} from '@app/features/input/state/InputKeybind';
import {formatKeyCombo} from '@app/features/input/utils/KeybindUtils';
import {isApplePlatform} from '@app/features/platform/utils/PlatformHelpers';
import styles from '@app/features/theme_settings/components/QuickCssEditor.module.css';
import {TURN_CUSTOM_CSS_ON_OR_OFF_DESCRIPTOR} from '@app/features/theme_settings/utils/ThemeSettingsMessageDescriptors';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {KeybindHint} from '@app/features/ui/keybind_hint/KeybindHint';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const TITLE_DESCRIPTOR = msg({
	message: 'Keyboard shortcuts',
	comment: 'Title of the dialog that lists the keyboard shortcuts of the quick CSS editor.',
});
const LEAVE_EDITOR_DESCRIPTOR = msg({
	message: 'Tab indents the line. To leave the editor with the keyboard, press Escape and then Tab.',
	comment: 'Shown at the top of the keyboard shortcuts dialog of the quick CSS editor.',
});
const OR_DESCRIPTOR = msg({
	message: 'or',
	comment: 'Between two keyboard shortcuts that do the same thing, in the shortcuts dialog of the quick CSS editor.',
});

interface ShortcutRow {
	description: MessageDescriptor;
	combos: ReadonlyArray<KeyCombo>;
}

const APPLE = isApplePlatform();
const mod = (key: string, extra: Partial<KeyCombo> = {}): KeyCombo => ({key, ctrlOrMeta: true, ...extra});

const ROWS: ReadonlyArray<ShortcutRow> = [
	{
		description: msg({
			message: 'Save the theme or apply changes',
			comment: 'Row in the quick CSS editor shortcuts dialog.',
		}),
		combos: [mod('s')],
	},
	{
		description: msg({message: 'Format the CSS', comment: 'Row in the quick CSS editor shortcuts dialog.'}),
		combos: [mod('f', {shift: true})],
	},
	{
		description: msg({message: 'Show problems', comment: 'Row in the quick CSS editor shortcuts dialog.'}),
		combos: [mod('m', {shift: true})],
	},
	{
		description: msg({message: 'Show the outline', comment: 'Row in the quick CSS editor shortcuts dialog.'}),
		combos: [mod('o', {shift: true})],
	},
	{
		description: msg({
			message: 'Pick an element in the app',
			comment: 'Row in the quick CSS editor shortcuts dialog.',
		}),
		combos: [mod('c', {shift: true})],
	},
	{
		description: msg({
			message: 'Pick the parent element while picking',
			comment: 'Row in the quick CSS editor shortcuts dialog.',
		}),
		combos: [{key: 'ArrowUp', alt: true}],
	},
	{
		description: msg({
			message: 'Pick a child element while picking',
			comment: 'Row in the quick CSS editor shortcuts dialog.',
		}),
		combos: [{key: 'ArrowDown', alt: true}],
	},
	{
		description: msg({
			message: 'Pick the next or previous element while picking',
			comment: 'Row in the quick CSS editor shortcuts dialog.',
		}),
		combos: [
			{key: 'ArrowRight', alt: true},
			{key: 'ArrowLeft', alt: true},
		],
	},
	{
		description: msg({
			message: 'Show details for the code at the cursor',
			comment:
				'Row in the quick CSS editor shortcuts dialog. Shows the same details as hovering, such as how many elements a selector matches.',
		}),
		combos: [mod('i')],
	},
	{
		description: msg({
			message: 'Edit the color at the cursor',
			comment:
				'Row in the quick CSS editor shortcuts dialog. Opens the color picker for the CSS color under the cursor.',
		}),
		combos: [{key: 'Enter', alt: true}],
	},
	{
		description: msg({message: 'Show completions', comment: 'Row in the quick CSS editor shortcuts dialog.'}),
		combos: [{key: ' ', ctrl: true}],
	},
	{
		description: msg({
			message: 'Change the number under the cursor by 1',
			comment: 'Row in the quick CSS editor shortcuts dialog.',
		}),
		combos: [
			{key: 'ArrowUp', alt: true},
			{key: 'ArrowDown', alt: true},
		],
	},
	{
		description: msg({
			message: 'Change the number under the cursor by 10',
			comment: 'Row in the quick CSS editor shortcuts dialog.',
		}),
		combos: [
			{key: 'ArrowUp', alt: true, shift: true},
			{key: 'ArrowDown', alt: true, shift: true},
		],
	},
	{
		description: msg({
			message: 'Change the number under the cursor by 0.1',
			comment: 'Row in the quick CSS editor shortcuts dialog.',
		}),
		combos: [
			{key: 'ArrowUp', alt: true, ctrl: true},
			{key: 'ArrowDown', alt: true, ctrl: true},
		],
	},
	{
		description: msg({
			message: "Move the line up or down when the cursor isn't on a number",
			comment: 'Row in the quick CSS editor shortcuts dialog.',
		}),
		combos: [
			{key: 'ArrowUp', alt: true},
			{key: 'ArrowDown', alt: true},
		],
	},
	{
		description: msg({message: 'Find and replace', comment: 'Row in the quick CSS editor shortcuts dialog.'}),
		combos: [mod('f')],
	},
	{
		description: msg({message: 'Go to a line', comment: 'Row in the quick CSS editor shortcuts dialog.'}),
		combos: [mod('g', {alt: true})],
	},
	{
		description: msg({
			message: 'Select the next match of the selection',
			comment: 'Row in the quick CSS editor shortcuts dialog.',
		}),
		combos: [mod('d')],
	},
	{
		description: msg({message: 'Add a cursor', comment: 'Row in the quick CSS editor shortcuts dialog.'}),
		combos: [{key: '', ctrlOrMeta: true, mouseButton: 0}],
	},
	{
		description: msg({
			message: 'Select a column of text by dragging',
			comment: 'Row in the quick CSS editor shortcuts dialog.',
		}),
		combos: [{key: '', alt: true, mouseButton: 0}],
	},
	{
		description: msg({message: 'Turn a comment on or off', comment: 'Row in the quick CSS editor shortcuts dialog.'}),
		combos: [mod('/')],
	},
	{
		description: msg({message: 'Fold the block', comment: 'Row in the quick CSS editor shortcuts dialog.'}),
		combos: [APPLE ? {key: '[', meta: true, alt: true} : {key: '[', ctrl: true, shift: true}],
	},
	{
		description: msg({message: 'Unfold the block', comment: 'Row in the quick CSS editor shortcuts dialog.'}),
		combos: [APPLE ? {key: ']', meta: true, alt: true} : {key: ']', ctrl: true, shift: true}],
	},
	{
		description: msg({message: 'Undo', comment: 'Row in the quick CSS editor shortcuts dialog.'}),
		combos: [mod('z')],
	},
	{
		description: msg({message: 'Redo', comment: 'Row in the quick CSS editor shortcuts dialog.'}),
		combos: [APPLE ? mod('z', {shift: true}) : mod('y')],
	},
];

const MOVE_FOCUS_DESCRIPTOR = msg({
	message: 'Move between the editor and the app',
	comment: 'Row in the quick CSS editor shortcuts dialog.',
});

const ShortcutKeys: React.FC<{combo: KeyCombo}> = ({combo}) => {
	const {i18n} = useLingui();
	if (combo.mouseButton !== undefined) {
		return (
			<span className={styles.shortcutText} data-flx="theme-settings.quick-css-shortcuts-modal.mouse">
				{formatKeyCombo(i18n, combo)}
			</span>
		);
	}
	return <KeybindHint combo={combo} data-flx="theme-settings.quick-css-shortcuts-modal.keybind-hint" />;
};

export const QuickCssShortcutsModal: React.FC = observer(() => {
	const {i18n} = useLingui();
	const focusCombos = Keybind.getActiveCombosForAction('system_open_theme_studio_popout');
	const customCssCombos = Keybind.getActiveCombosForAction('system_toggle_custom_css');
	const rows: ReadonlyArray<ShortcutRow> = [
		...ROWS,
		...(focusCombos.length > 0 ? [{description: MOVE_FOCUS_DESCRIPTOR, combos: focusCombos.slice(0, 1)}] : []),
		...(customCssCombos.length > 0
			? [{description: TURN_CUSTOM_CSS_ON_OR_OFF_DESCRIPTOR, combos: customCssCombos.slice(0, 1)}]
			: []),
	];
	return (
		<Modal.Root
			size="small"
			centered
			onClose={ModalCommands.pop}
			data-flx="theme-settings.quick-css-shortcuts-modal.root"
		>
			<Modal.Header title={i18n._(TITLE_DESCRIPTOR)} data-flx="theme-settings.quick-css-shortcuts-modal.header" />
			<Modal.Content data-flx="theme-settings.quick-css-shortcuts-modal.content">
				<Modal.ContentLayout data-flx="theme-settings.quick-css-shortcuts-modal.layout">
					<Modal.Description data-flx="theme-settings.quick-css-shortcuts-modal.description">
						{i18n._(LEAVE_EDITOR_DESCRIPTOR)}
					</Modal.Description>
					<dl className={styles.shortcuts} data-flx="theme-settings.quick-css-shortcuts-modal.list">
						{rows.map((row) => (
							<div
								key={row.description.id}
								className={styles.shortcutRow}
								data-flx="theme-settings.quick-css-shortcuts-modal.row"
							>
								<dt data-flx="theme-settings.quick-css-shortcuts-modal.term">{i18n._(row.description)}</dt>
								<dd className={styles.shortcutKeys} data-flx="theme-settings.quick-css-shortcuts-modal.keys">
									{row.combos.map((combo, index) => (
										<span
											key={index}
											className={styles.shortcutCombo}
											data-flx="theme-settings.quick-css-shortcuts-modal.combo"
										>
											{index > 0 ? (
												<span className={styles.shortcutText} data-flx="theme-settings.quick-css-shortcuts-modal.or">
													{i18n._(OR_DESCRIPTOR)}
												</span>
											) : null}
											<ShortcutKeys combo={combo} data-flx="theme-settings.quick-css-shortcuts-modal.shortcut-keys" />
										</span>
									))}
								</dd>
							</div>
						))}
					</dl>
				</Modal.ContentLayout>
			</Modal.Content>
		</Modal.Root>
	);
});
