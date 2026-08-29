// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import Spellcheck from '@app/features/messaging/state/Spellcheck';
import {isEditableTextInput, replaceSelectedText} from '@app/features/messaging/utils/TextInputEditUtils';
import type {SpellcheckEngine} from '@app/features/platform/types/Electron';
import {CheckboxItem, MenuSeparator} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemRadio} from '@app/features/ui/action_menu/MenuItemRadio';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {getElectronAPI, isElectron} from '@app/features/ui/utils/NativeUtils';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {
	ArrowClockwiseIcon,
	ArrowCounterClockwiseIcon,
	ClipboardTextIcon,
	CopyIcon,
	ScissorsIcon,
	SelectionIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const REMOVE_FROM_DICTIONARY_DESCRIPTOR = msg({
	message: 'Remove from dictionary',
	comment:
		'Textarea right-click context menu item that removes the selected misspelled word from the personal dictionary.',
});
const ADD_TO_DICTIONARY_DESCRIPTOR = msg({
	message: 'Add to dictionary',
	comment: 'Textarea right-click context menu item that adds the selected misspelled word to the personal dictionary.',
});
const UNDO_DESCRIPTOR = msg({
	message: 'Undo',
	comment: 'Textarea right-click context menu item that undoes the last edit.',
});
const REDO_DESCRIPTOR = msg({
	message: 'Redo',
	comment: 'Textarea right-click context menu item that redoes the last undone edit.',
});
const CUT_DESCRIPTOR = msg({
	message: 'Cut',
	comment: 'Textarea right-click context menu item that cuts the selected text.',
});
const COPY_DESCRIPTOR = msg({
	message: 'Copy',
	comment: 'Textarea right-click context menu item that copies the selected text.',
});
const PASTE_DESCRIPTOR = msg({
	message: 'Paste',
	comment: 'Textarea right-click context menu item that pastes clipboard contents.',
});
const SELECT_ALL_DESCRIPTOR = msg({
	message: 'Select all',
	comment: 'Textarea right-click context menu item that selects all text in the textarea.',
});
const SPELLCHECK_DESCRIPTOR = msg({
	message: 'Spellcheck',
	comment: 'Submenu label in the textarea right-click context menu for spellcheck preferences.',
});
const ENABLED_DESCRIPTOR = msg({
	message: 'Enabled',
	comment: 'Toggle item in the textarea spellcheck submenu. Master enable for spellcheck.',
});
const AUTO_DESCRIPTOR = msg({
	message: 'Recommended',
	comment:
		'Spellcheck engine option in the textarea spellcheck submenu. Lets the app choose the best available engine.',
});
const IN_APP_HUNSPELL_DESCRIPTOR = msg({
	message: 'In-app dictionaries',
	comment: 'Spellcheck engine option in the textarea spellcheck submenu. The bundled in-app hunspell engine.',
});
const OPERATING_SYSTEM_DESCRIPTOR = msg({
	message: 'Operating system',
	comment: 'Spellcheck engine option in the textarea spellcheck submenu. Uses the host operating system spellchecker.',
});
const AUTO_DETECT_LANGUAGE_DESCRIPTOR = msg({
	message: 'Detect while typing',
	comment:
		'Item in the textarea spellcheck language submenu that detects draft message language and switches dictionaries as the user types.',
});
const LANGUAGES_DESCRIPTOR = msg({
	message: 'Languages',
	comment: 'Submenu label in the textarea spellcheck submenu listing manual language overrides.',
});
const RELOAD_TO_APPLY_ENGINE_CHANGE_DESCRIPTOR = msg({
	message: 'Reload to apply engine change',
	comment: 'Hint in the textarea spellcheck submenu shown after switching engines.',
});
const SPELLCHECK_SETTINGS_DESCRIPTOR = msg({
	message: '{productName} spellcheck settings…',
	comment:
		'Item in the textarea spellcheck submenu that opens the full settings page. Trailing horizontal ellipsis (…) indicates the action opens settings.',
});

const SHOW_SEND_BUTTON_DESCRIPTOR = msg({
	message: 'Show send button',
	comment: 'Short label in the channel and chat textarea plus menu. Keep it concise.',
});

export interface TextareaContextMenuEditFlags {
	canUndo: boolean;
	canRedo: boolean;
	canCut: boolean;
	canCopy: boolean;
	canPaste: boolean;
	canSelectAll: boolean;
}

interface TextareaContextMenuProps {
	misspelledWord?: string;
	suggestions?: Array<string>;
	editFlags?: TextareaContextMenuEditFlags;
	targetElement?: HTMLElement | null;
	showSendButtonToggle?: boolean;
	onClose: () => void;
}

export const TextareaContextMenu = observer(
	({
		misspelledWord,
		suggestions = [],
		editFlags,
		targetElement,
		showSendButtonToggle = false,
		onClose,
	}: TextareaContextMenuProps) => {
		const {i18n} = useLingui();
		const showMessageSendButton = Accessibility.showMessageSendButton;
		const electronAPI = isElectron() ? getElectronAPI() : null;
		const focusTargetElement = () => {
			if (!targetElement?.isConnected) {
				return;
			}
			targetElement.focus({preventScroll: true});
		};
		const handleReplaceMisspelling = async (suggestion: string) => {
			if (electronAPI?.spellcheckReplaceMisspelling) {
				await electronAPI.spellcheckReplaceMisspelling(suggestion);
			}
			onClose();
		};
		const handleAddToDictionary = async () => {
			if (!misspelledWord) return;
			Spellcheck.addPersonalWord(misspelledWord);
			if (electronAPI?.spellcheckAddWordToDictionary) {
				await electronAPI.spellcheckAddWordToDictionary(misspelledWord);
			}
			onClose();
		};
		const handleRemoveFromDictionary = () => {
			if (!misspelledWord) return;
			Spellcheck.removePersonalWord(misspelledWord);
			onClose();
		};
		const isInPersonalDict = misspelledWord ? Spellcheck.personalDictionary.includes(misspelledWord) : false;
		const runAfterClose = (action: () => void) => {
			onClose();
			requestAnimationFrame(() => {
				focusTargetElement();
				requestAnimationFrame(action);
			});
		};
		const execCommand = (command: string) => {
			runAfterClose(() => {
				document.execCommand(command);
			});
		};
		const insertTextInActiveEditable = (text: string): boolean => {
			if (!text) return true;
			const active = document.activeElement;
			if (isEditableTextInput(active)) {
				return replaceSelectedText(active, text);
			}
			return false;
		};
		const handlePaste = () => {
			runAfterClose(() => {
				if (electronAPI?.pasteFromClipboard) {
					void electronAPI.pasteFromClipboard();
					return;
				}
				const readClipboard = electronAPI?.clipboardReadText;
				if (readClipboard) {
					void readClipboard()
						.then((text: string) => {
							insertTextInActiveEditable(text);
						})
						.catch(() => {});
					return;
				}
				if (navigator.clipboard?.readText) {
					void navigator.clipboard.readText().then((text) => {
						insertTextInActiveEditable(text);
					});
					return;
				}
			});
		};
		const handleOpenSpellcheckSettings = () => {
			ModalCommands.push(
				ModalCommands.modal(() => (
					<UserSettingsModal
						initialTab="language"
						data-flx="channel.textarea.textarea-context-menu.handle-open-spellcheck-settings.user-settings-modal"
					/>
				)),
			);
			onClose();
		};
		const spellcheckEnabled = Spellcheck.enabled;
		const hasMisspelling = spellcheckEnabled && misspelledWord && suggestions.length > 0;
		return (
			<>
				{hasMisspelling && (
					<>
						<MenuGroup data-flx="channel.textarea.textarea-context-menu.menu-group">
							{suggestions.slice(0, 6).map((suggestion) => (
								<MenuItem
									key={suggestion}
									onClick={() => handleReplaceMisspelling(suggestion)}
									data-flx="channel.textarea.textarea-context-menu.menu-item.replace-misspelling"
								>
									{suggestion}
								</MenuItem>
							))}
						</MenuGroup>
						<MenuGroup data-flx="channel.textarea.textarea-context-menu.menu-group--2">
							{isInPersonalDict ? (
								<MenuItem
									onClick={handleRemoveFromDictionary}
									data-flx="channel.textarea.textarea-context-menu.menu-item.remove-from-dictionary"
								>
									{i18n._(REMOVE_FROM_DICTIONARY_DESCRIPTOR)}
								</MenuItem>
							) : (
								<MenuItem
									onClick={handleAddToDictionary}
									data-flx="channel.textarea.textarea-context-menu.menu-item.add-to-dictionary"
								>
									{i18n._(ADD_TO_DICTIONARY_DESCRIPTOR)}
								</MenuItem>
							)}
						</MenuGroup>
					</>
				)}
				<MenuGroup data-flx="channel.textarea.textarea-context-menu.menu-group--3">
					<MenuItem
						icon={
							<ArrowCounterClockwiseIcon data-flx="channel.textarea.textarea-context-menu.arrow-counter-clockwise-icon" />
						}
						onClick={() => execCommand('undo')}
						disabled={!editFlags?.canUndo}
						data-flx="channel.textarea.textarea-context-menu.menu-item.exec-command"
					>
						{i18n._(UNDO_DESCRIPTOR)}
					</MenuItem>
					<MenuItem
						icon={<ArrowClockwiseIcon data-flx="channel.textarea.textarea-context-menu.arrow-clockwise-icon" />}
						onClick={() => execCommand('redo')}
						disabled={!editFlags?.canRedo}
						data-flx="channel.textarea.textarea-context-menu.menu-item.exec-command--2"
					>
						{i18n._(REDO_DESCRIPTOR)}
					</MenuItem>
				</MenuGroup>
				<MenuGroup data-flx="channel.textarea.textarea-context-menu.menu-group--4">
					<MenuItem
						icon={<ScissorsIcon data-flx="channel.textarea.textarea-context-menu.scissors-icon" />}
						onClick={() => execCommand('cut')}
						disabled={!editFlags?.canCut}
						data-flx="channel.textarea.textarea-context-menu.menu-item.exec-command--3"
					>
						{i18n._(CUT_DESCRIPTOR)}
					</MenuItem>
					<MenuItem
						icon={<CopyIcon data-flx="channel.textarea.textarea-context-menu.copy-icon" />}
						onClick={() => execCommand('copy')}
						disabled={!editFlags?.canCopy}
						data-flx="channel.textarea.textarea-context-menu.menu-item.exec-command--4"
					>
						{i18n._(COPY_DESCRIPTOR)}
					</MenuItem>
					<MenuItem
						icon={<ClipboardTextIcon data-flx="channel.textarea.textarea-context-menu.clipboard-text-icon" />}
						onClick={() => handlePaste()}
						disabled={!editFlags?.canPaste}
						data-flx="channel.textarea.textarea-context-menu.menu-item.paste"
					>
						{i18n._(PASTE_DESCRIPTOR)}
					</MenuItem>
					<MenuItem
						icon={<SelectionIcon data-flx="channel.textarea.textarea-context-menu.selection-icon" />}
						onClick={() => execCommand('selectAll')}
						disabled={!editFlags?.canSelectAll}
						data-flx="channel.textarea.textarea-context-menu.menu-item.exec-command--5"
					>
						{i18n._(SELECT_ALL_DESCRIPTOR)}
					</MenuItem>
				</MenuGroup>
				<MenuGroup data-flx="channel.textarea.textarea-context-menu.menu-group--5">
					<MenuItemSubmenu
						label={i18n._(SPELLCHECK_DESCRIPTOR)}
						render={() => (
							<SpellcheckSubmenu
								isElectron={isElectron()}
								onOpenSpellcheckSettings={handleOpenSpellcheckSettings}
								data-flx="channel.textarea.textarea-context-menu.spellcheck-submenu"
							/>
						)}
						data-flx="channel.textarea.textarea-context-menu.menu-item-submenu"
					/>
				</MenuGroup>
				{showSendButtonToggle && (
					<MenuGroup data-flx="channel.textarea.textarea-context-menu.menu-group--6">
						<CheckboxItem
							checked={showMessageSendButton}
							onCheckedChange={(checked) => AccessibilityCommands.update({showMessageSendButton: checked})}
							closeOnChange={false}
							data-flx="channel.textarea.textarea-context-menu.checkbox-item.show-send-button"
						>
							{i18n._(SHOW_SEND_BUTTON_DESCRIPTOR)}
						</CheckboxItem>
					</MenuGroup>
				)}
			</>
		);
	},
);

interface SpellcheckSubmenuProps {
	isElectron: boolean;
	onOpenSpellcheckSettings: () => void;
}

const SpellcheckSubmenu = observer(({isElectron: electron, onOpenSpellcheckSettings}: SpellcheckSubmenuProps) => {
	const {i18n} = useLingui();
	const enabled = Spellcheck.enabled;
	if (!electron) {
		return (
			<CheckboxItem
				checked={enabled}
				onCheckedChange={(value) => Spellcheck.setEnabled(value)}
				data-flx="channel.textarea.textarea-context-menu.spellcheck-submenu.checkbox-item"
			>
				{i18n._(ENABLED_DESCRIPTOR)}
			</CheckboxItem>
		);
	}
	const engineLabels: Record<SpellcheckEngine, string> = {
		auto: i18n._(AUTO_DESCRIPTOR),
		hunspell: i18n._(IN_APP_HUNSPELL_DESCRIPTOR),
		system: i18n._(OPERATING_SYSTEM_DESCRIPTOR),
	};
	const engineOrder: Array<SpellcheckEngine> = ['auto', 'hunspell', 'system'];
	const selectedTagSet = new Set(Spellcheck.languages.map((l) => l.toLowerCase()));
	const dictionaries = Spellcheck.bundledDictionaries;
	const showLanguagesSubmenu = !Spellcheck.autoDetect && dictionaries.length > 0;
	return (
		<>
			<CheckboxItem
				checked={enabled}
				onCheckedChange={(value) => Spellcheck.setEnabled(value)}
				data-flx="channel.textarea.textarea-context-menu.spellcheck-submenu.checkbox-item--2"
			>
				{i18n._(ENABLED_DESCRIPTOR)}
			</CheckboxItem>
			{enabled && (
				<>
					<MenuSeparator data-flx="channel.textarea.textarea-context-menu.spellcheck-submenu.menu-separator" />
					{engineOrder.map((engine) => (
						<MenuItemRadio
							key={engine}
							selected={Spellcheck.engine === engine}
							onSelect={() => Spellcheck.setEngine(engine)}
							data-flx="channel.textarea.textarea-context-menu.spellcheck-submenu.menu-item-radio.set-engine"
						>
							{engineLabels[engine]}
						</MenuItemRadio>
					))}
					<MenuSeparator data-flx="channel.textarea.textarea-context-menu.spellcheck-submenu.menu-separator--2" />
					<CheckboxItem
						checked={Spellcheck.autoDetect}
						onCheckedChange={(value) => Spellcheck.setAutoDetect(value)}
						data-flx="channel.textarea.textarea-context-menu.spellcheck-submenu.checkbox-item--3"
					>
						{i18n._(AUTO_DETECT_LANGUAGE_DESCRIPTOR)}
					</CheckboxItem>
					{showLanguagesSubmenu && (
						<MenuItemSubmenu
							label={i18n._(LANGUAGES_DESCRIPTOR)}
							render={() => (
								<>
									{dictionaries.map((dict) => {
										const checked = selectedTagSet.has(dict.tag.toLowerCase());
										return (
											<CheckboxItem
												key={`${dict.package}-${dict.tag}`}
												checked={checked}
												onCheckedChange={() => Spellcheck.toggleLanguage(dict.tag)}
												data-flx="channel.textarea.textarea-context-menu.spellcheck-submenu.checkbox-item--4"
											>
												{dict.nativeName} ({dict.tag})
											</CheckboxItem>
										);
									})}
								</>
							)}
							data-flx="channel.textarea.textarea-context-menu.spellcheck-submenu.menu-item-submenu"
						/>
					)}
					{Spellcheck.reloadRequired && (
						<>
							<MenuSeparator data-flx="channel.textarea.textarea-context-menu.spellcheck-submenu.menu-separator--3" />
							<MenuItem
								onClick={() => {
									if (typeof window !== 'undefined') window.location.reload();
								}}
								data-flx="channel.textarea.textarea-context-menu.spellcheck-submenu.menu-item"
							>
								{i18n._(RELOAD_TO_APPLY_ENGINE_CHANGE_DESCRIPTOR)}
							</MenuItem>
						</>
					)}
				</>
			)}
			<MenuSeparator data-flx="channel.textarea.textarea-context-menu.spellcheck-submenu.menu-separator--4" />
			<MenuItem
				onClick={onOpenSpellcheckSettings}
				data-flx="channel.textarea.textarea-context-menu.spellcheck-submenu.menu-item.open-spellcheck-settings"
			>
				{i18n._(SPELLCHECK_SETTINGS_DESCRIPTOR, {productName: PRODUCT_NAME})}
			</MenuItem>
		</>
	);
});
