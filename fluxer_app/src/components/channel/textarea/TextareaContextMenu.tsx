/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

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
import {MenuGroup} from '~/components/uikit/ContextMenu/MenuGroup';
import {MenuItem} from '~/components/uikit/ContextMenu/MenuItem';
import {MenuItemCheckbox} from '~/components/uikit/ContextMenu/MenuItemCheckbox';
import SpellcheckStore from '~/stores/SpellcheckStore';
import {getElectronAPI, isElectron} from '~/utils/NativeUtils';

interface TextareaContextMenuEditFlags {
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
	onClose: () => void;
}

export const TextareaContextMenu = observer(
	({misspelledWord, suggestions = [], editFlags, onClose}: TextareaContextMenuProps) => {
		const {t} = useLingui();
		const electronAPI = isElectron() ? getElectronAPI() : null;
		const showSpellMenu = isElectron() && (electronAPI?.platform === 'darwin' || electronAPI?.platform === 'win32');

		const handleReplaceMisspelling = async (suggestion: string) => {
			if (electronAPI) {
				await electronAPI.spellcheckReplaceMisspelling(suggestion);
			}
			onClose();
		};

		const handleAddToDictionary = async () => {
			if (!misspelledWord || !electronAPI) return;
			await electronAPI.spellcheckAddWordToDictionary(misspelledWord);
			onClose();
		};

		const execCommand = (command: string) => {
			document.execCommand(command);
			onClose();
		};

		const handleToggleSpellcheck = (checked: boolean) => {
			SpellcheckStore.setEnabled(checked);
		};

		const handleOpenLanguageSettings = async () => {
			if (!electronAPI) return;
			await electronAPI.spellcheckOpenLanguageSettings();
			onClose();
		};

		const spellcheckEnabled = SpellcheckStore.enabled;
		const hasMisspelling = spellcheckEnabled && misspelledWord && suggestions.length > 0;

		return (
			<>
				{hasMisspelling && (
					<>
						<MenuGroup>
							{suggestions.slice(0, 6).map((suggestion) => (
								<MenuItem key={suggestion} onClick={() => handleReplaceMisspelling(suggestion)}>
									{suggestion}
								</MenuItem>
							))}
						</MenuGroup>
						<MenuGroup>
							<MenuItem onClick={handleAddToDictionary}>{t`Add to Dictionary`}</MenuItem>
						</MenuGroup>
					</>
				)}

				<MenuGroup>
					<MenuItem
						icon={<ArrowCounterClockwiseIcon />}
						onClick={() => execCommand('undo')}
						disabled={!editFlags?.canUndo}
					>
						{t`Undo`}
					</MenuItem>
					<MenuItem icon={<ArrowClockwiseIcon />} onClick={() => execCommand('redo')} disabled={!editFlags?.canRedo}>
						{t`Redo`}
					</MenuItem>
				</MenuGroup>

				<MenuGroup>
					<MenuItem icon={<ScissorsIcon />} onClick={() => execCommand('cut')} disabled={!editFlags?.canCut}>
						{t`Cut`}
					</MenuItem>
					<MenuItem icon={<CopyIcon />} onClick={() => execCommand('copy')} disabled={!editFlags?.canCopy}>
						{t`Copy`}
					</MenuItem>
					<MenuItem icon={<ClipboardTextIcon />} onClick={() => execCommand('paste')} disabled={!editFlags?.canPaste}>
						{t`Paste`}
					</MenuItem>
					<MenuItem
						icon={<SelectionIcon />}
						onClick={() => execCommand('selectAll')}
						disabled={!editFlags?.canSelectAll}
					>
						{t`Select All`}
					</MenuItem>
				</MenuGroup>

				{showSpellMenu && (
					<MenuGroup>
						<MenuItemCheckbox checked={spellcheckEnabled} onChange={handleToggleSpellcheck}>
							{t`Spellcheck`}
						</MenuItemCheckbox>
						<MenuItem onClick={handleOpenLanguageSettings}>{t`Languages…`}</MenuItem>
					</MenuGroup>
				)}
			</>
		);
	},
);
