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
import {observer} from 'mobx-react-lite';
import React, {useEffect, useMemo, useState} from 'react';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import {Permissions} from '~/Constants';
import {createMessageActionHandlers, useMessagePermissions} from '~/components/channel/messageActionUtils';
import type {MessageRecord} from '~/records/MessageRecord';
import PermissionStore from '~/stores/PermissionStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import {openExternalUrl} from '~/utils/NativeUtils';
import {CopyIcon, CopyLinkIcon, OpenLinkIcon} from './ContextMenuIcons';
import {
	AddReactionMenuItem,
	BookmarkMessageMenuItem,
	CopyMessageIdMenuItem,
	CopyMessageLinkMenuItem,
	CopyMessageTextMenuItem,
	DebugMessageMenuItem,
	DeleteMessageMenuItem,
	EditMessageMenuItem,
	ForwardMessageMenuItem,
	MarkAsUnreadMenuItem,
	PinMessageMenuItem,
	RemoveAllReactionsMenuItem,
	ReplyMessageMenuItem,
	SpeakMessageMenuItem,
	SuppressEmbedsMenuItem,
} from './items/MessageMenuItems';
import {ReportMessageMenuItem} from './items/ReportMessageMenuItem';
import {MenuGroup} from './MenuGroup';
import {MenuItem} from './MenuItem';

interface SelectionSnapshot {
	text: string;
	range: Range | null;
}

const getSelectionSnapshot = (): SelectionSnapshot => {
	const selection = window.getSelection();
	if (!selection || selection.rangeCount === 0) {
		return {text: '', range: null};
	}

	const text = selection.toString().trim();
	if (!text) {
		return {text: '', range: null};
	}

	try {
		return {text, range: selection.getRangeAt(0).cloneRange()};
	} catch {
		return {text, range: null};
	}
};

interface MessageContextMenuProps {
	message: MessageRecord;
	onClose: () => void;
	onDelete: (bypassConfirm?: boolean) => void;
	linkUrl?: string;
}

export const MessageContextMenu: React.FC<MessageContextMenuProps> = observer(
	({message, onClose, onDelete, linkUrl}) => {
		const {i18n} = useLingui();
		const [{text: initialSelectionText, range: initialSelectionRange}] = useState(getSelectionSnapshot);
		const [selectionText, setSelectionText] = useState(initialSelectionText);
		const savedSelectionRangeRef = React.useRef<Range | null>(initialSelectionRange);
		const restoringSelectionRef = React.useRef(false);

		const restoreSelection = React.useCallback(() => {
			const savedRange = savedSelectionRangeRef.current;
			if (!savedRange) return;

			const selection = window.getSelection();
			if (!selection) return;

			try {
				const rangeForSelection = savedRange.cloneRange();
				const rangeForStorage = rangeForSelection.cloneRange();

				restoringSelectionRef.current = true;
				selection.removeAllRanges();
				selection.addRange(rangeForSelection);
				savedSelectionRangeRef.current = rangeForStorage;
				setSelectionText(rangeForStorage.toString().trim());
			} catch {
				savedSelectionRangeRef.current = null;
				return;
			} finally {
				window.requestAnimationFrame(() => {
					restoringSelectionRef.current = false;
				});
			}
		}, []);

		React.useLayoutEffect(() => {
			if (!savedSelectionRangeRef.current) return;
			restoreSelection();
		}, [restoreSelection]);

		useEffect(() => {
			const handleSelectionChange = () => {
				if (restoringSelectionRef.current) return;

				const {text, range} = getSelectionSnapshot();
				if (text) {
					savedSelectionRangeRef.current = range;
					setSelectionText(text);
					return;
				}

				if (savedSelectionRangeRef.current) {
					restoreSelection();
					return;
				}

				setSelectionText('');
			};

			document.addEventListener('selectionchange', handleSelectionChange);
			return () => document.removeEventListener('selectionchange', handleSelectionChange);
		}, [restoreSelection]);

		const copyShortcut = useMemo(() => {
			return /Mac|iPod|iPhone|iPad/.test(navigator.platform) ? '⌘C' : 'Ctrl+C';
		}, []);

		const handleCopySelection = React.useCallback(async () => {
			if (!selectionText) return;
			await TextCopyActionCreators.copy(i18n, selectionText, true);
			onClose();
		}, [selectionText, onClose, i18n]);

		const handleCopyLink = React.useCallback(async () => {
			if (!linkUrl) return;
			await TextCopyActionCreators.copy(i18n, linkUrl, true);
			onClose();
		}, [linkUrl, onClose, i18n]);

		const handleOpenLink = React.useCallback(() => {
			if (!linkUrl) return;
			void openExternalUrl(linkUrl);
			onClose();
		}, [linkUrl, onClose]);

		const {
			canSendMessages,
			canAddReactions,
			canEditMessage,
			canDeleteMessage,
			canPinMessage,
			shouldRenderSuppressEmbeds,
			isDM,
		} = useMessagePermissions(message);

		const canManageMessages = !isDM && PermissionStore.can(Permissions.MANAGE_MESSAGES, {channelId: message.channelId});

		const handlers = createMessageActionHandlers(message);
		const developerMode = UserSettingsStore.developerMode;

		return (
			<>
				{linkUrl && (
					<MenuGroup>
						<MenuItem icon={<OpenLinkIcon />} onClick={handleOpenLink}>
							Open Link
						</MenuItem>
						<MenuItem icon={<CopyLinkIcon />} onClick={handleCopyLink}>
							Copy Link
						</MenuItem>
					</MenuGroup>
				)}

				{selectionText && (
					<MenuGroup>
						<MenuItem icon={<CopyIcon />} onClick={handleCopySelection} shortcut={copyShortcut}>
							Copy
						</MenuItem>
					</MenuGroup>
				)}

				<MenuGroup>
					{canAddReactions && <AddReactionMenuItem message={message} onClose={onClose} />}

					<MarkAsUnreadMenuItem message={message} onMarkAsUnread={handlers.handleMarkAsUnread} onClose={onClose} />

					{message.isUserMessage() && !message.messageSnapshots && canEditMessage && (
						<EditMessageMenuItem message={message} onEdit={handlers.handleEditMessage} onClose={onClose} />
					)}

					{message.isUserMessage() && canSendMessages && (
						<ReplyMessageMenuItem message={message} onReply={handlers.handleReply} onClose={onClose} />
					)}

					{message.isUserMessage() && (
						<ForwardMessageMenuItem message={message} onForward={handlers.handleForward} onClose={onClose} />
					)}
				</MenuGroup>

				{(message.isUserMessage() || message.content) && (
					<MenuGroup>
						{message.isUserMessage() && (
							<BookmarkMessageMenuItem message={message} onSave={handlers.handleSaveMessage} onClose={onClose} />
						)}

						{message.isUserMessage() && canPinMessage && (
							<PinMessageMenuItem message={message} onPin={handlers.handlePinMessage} onClose={onClose} />
						)}

						{shouldRenderSuppressEmbeds && (
							<SuppressEmbedsMenuItem
								message={message}
								onToggleSuppressEmbeds={handlers.handleToggleSuppressEmbeds}
								onClose={onClose}
							/>
						)}

						{message.content && (
							<CopyMessageTextMenuItem message={message} onCopyMessage={handlers.handleCopyMessage} onClose={onClose} />
						)}

						<SpeakMessageMenuItem message={message} onClose={onClose} />
					</MenuGroup>
				)}

				<MenuGroup>
					<CopyMessageLinkMenuItem
						message={message}
						onCopyMessageLink={handlers.handleCopyMessageLink}
						onClose={onClose}
					/>

					<CopyMessageIdMenuItem message={message} onCopyMessageId={handlers.handleCopyMessageId} onClose={onClose} />

					{developerMode && <DebugMessageMenuItem message={message} onClose={onClose} />}
				</MenuGroup>

				{!message.isCurrentUserAuthor() && (
					<MenuGroup>
						<ReportMessageMenuItem message={message} onClose={onClose} />
					</MenuGroup>
				)}

				{canDeleteMessage && (
					<MenuGroup>
						<DeleteMessageMenuItem message={message} onDelete={onDelete} onClose={onClose} />
					</MenuGroup>
				)}

				{canManageMessages && message.reactions.length > 0 && (
					<MenuGroup>
						<RemoveAllReactionsMenuItem
							message={message}
							onRemoveAllReactions={handlers.handleRemoveAllReactions}
							onClose={onClose}
						/>
					</MenuGroup>
				)}
			</>
		);
	},
);
