// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TextualPreviewContextMenuProps} from '@app/features/channel/components/embeds/attachments/TextualAttachmentPreviewUtils';
import {DOWNLOAD_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {DownloadSimpleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const WRAP_TEXT_DESCRIPTOR = msg({
	message: 'Wrap text',
	comment: 'Short label in the channel and chat textual preview context menu. Keep it concise.',
});
export const TextualPreviewContextMenu = observer(function TextualPreviewContextMenu({
	onDownload,
	onToggleWrapText,
	showWrapText = true,
	wrapText,
}: TextualPreviewContextMenuProps) {
	const {i18n} = useLingui();
	return (
		<MenuGroup data-flx="channel.embeds.attachments.textual-preview-context-menu.menu-group">
			<MenuItem
				icon={
					<DownloadSimpleIcon
						size={remFromPx(16)}
						weight="regular"
						data-flx="channel.embeds.attachments.textual-preview-context-menu.download-simple-icon"
					/>
				}
				onClick={onDownload}
				data-flx="channel.embeds.attachments.textual-preview-context-menu.menu-item.download"
			>
				{i18n._(DOWNLOAD_DESCRIPTOR)}
			</MenuItem>
			{showWrapText && (
				<CheckboxItem
					checked={wrapText}
					onCheckedChange={onToggleWrapText}
					data-flx="channel.embeds.attachments.textual-preview-context-menu.checkbox-item"
				>
					{i18n._(WRAP_TEXT_DESCRIPTOR)}
				</CheckboxItem>
			)}
		</MenuGroup>
	);
});
