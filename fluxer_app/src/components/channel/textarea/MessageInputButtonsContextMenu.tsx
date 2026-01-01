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
import {ClockIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import * as AccessibilityActionCreators from '~/actions/AccessibilityActionCreators';
import {MenuGroup} from '~/components/uikit/ContextMenu/MenuGroup';
import {MenuItem} from '~/components/uikit/ContextMenu/MenuItem';
import {MenuItemCheckbox} from '~/components/uikit/ContextMenu/MenuItemCheckbox';
import AccessibilityStore from '~/stores/AccessibilityStore';

import styles from './MessageInputButtonsContextMenu.module.css';

export interface MessageInputButtonsContextMenuProps {
	onSchedule?: () => void;
	canSchedule?: boolean;
}

export const MessageInputButtonsContextMenu = observer(
	({canSchedule = false, onSchedule}: MessageInputButtonsContextMenuProps) => {
		const {t} = useLingui();
		const showUploadButton = AccessibilityStore.showUploadButton;
		const showGiftButton = AccessibilityStore.showGiftButton;
		const showGifButton = AccessibilityStore.showGifButton;
		const showMemesButton = AccessibilityStore.showMemesButton;
		const showStickersButton = AccessibilityStore.showStickersButton;
		const showEmojiButton = AccessibilityStore.showEmojiButton;
		const showMessageSendButton = AccessibilityStore.showMessageSendButton;

		return (
			<>
				<MenuGroup>
					<MenuItem disabled={true} className={styles.headingItem}>
						{t`Visible Buttons`}
					</MenuItem>
					<MenuItemCheckbox
						checked={showUploadButton}
						onChange={(checked) => AccessibilityActionCreators.update({showUploadButton: checked})}
					>
						{t`Show Upload Button`}
					</MenuItemCheckbox>
					<MenuItemCheckbox
						checked={showGiftButton}
						onChange={(checked) => AccessibilityActionCreators.update({showGiftButton: checked})}
					>
						{t`Show Gift Button`}
					</MenuItemCheckbox>
					<MenuItemCheckbox
						checked={showGifButton}
						onChange={(checked) => AccessibilityActionCreators.update({showGifButton: checked})}
					>
						{t`Show GIFs Button`}
					</MenuItemCheckbox>
					<MenuItemCheckbox
						checked={showMemesButton}
						onChange={(checked) => AccessibilityActionCreators.update({showMemesButton: checked})}
					>
						{t`Show Memes Button`}
					</MenuItemCheckbox>
					<MenuItemCheckbox
						checked={showStickersButton}
						onChange={(checked) => AccessibilityActionCreators.update({showStickersButton: checked})}
					>
						{t`Show Stickers Button`}
					</MenuItemCheckbox>
					<MenuItemCheckbox
						checked={showEmojiButton}
						onChange={(checked) => AccessibilityActionCreators.update({showEmojiButton: checked})}
					>
						{t`Show Emoji Button`}
					</MenuItemCheckbox>
					<MenuItemCheckbox
						checked={showMessageSendButton}
						onChange={(checked) => AccessibilityActionCreators.update({showMessageSendButton: checked})}
					>
						{t`Show Send Button`}
					</MenuItemCheckbox>
				</MenuGroup>
				{canSchedule && (
					<MenuGroup>
						<MenuItem
							onClick={() => {
								onSchedule?.();
							}}
						>
							<ClockIcon className={styles.icon} />
							{t`Schedule Message`}
						</MenuItem>
					</MenuGroup>
				)}
			</>
		);
	},
);
