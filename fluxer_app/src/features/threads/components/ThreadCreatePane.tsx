// SPDX-License-Identifier: AGPL-3.0-or-later

import {Message as MessageComponent} from '@app/features/channel/components/ChannelMessage';
import {LexicalChannelTextareaContent} from '@app/features/channel/components/LexicalChannelTextareaContent';
import type {Channel} from '@app/features/channel/models/Channel';
import * as DraftCommands from '@app/features/messaging/commands/DraftCommands';
import type {SendMessageFunction} from '@app/features/messaging/hooks/useMessageSubmission';
import Drafts from '@app/features/messaging/state/MessagingDrafts';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {CloudUpload} from '@app/features/messaging/upload/CloudUpload';
import * as MessageSubmitUtils from '@app/features/messaging/utils/MessageSubmitUtils';
import {formatUploadingAttachmentSummary} from '@app/features/messaging/utils/UploadingAttachmentLabelUtils';
import Permission from '@app/features/permissions/state/Permission';
import * as ThreadCommands from '@app/features/threads/commands/ThreadCommands';
import {openThread} from '@app/features/threads/commands/ThreadNavigation';
import styles from '@app/features/threads/components/ThreadCreatePane.module.css';
import ThreadPanel from '@app/features/threads/state/ThreadPanel';
import {canCreateThreadIn} from '@app/features/threads/utils/ThreadActionRules';
import * as D from '@app/features/threads/utils/ThreadMessageDescriptors';
import {sendThreadStarter} from '@app/features/threads/utils/ThreadStarterSend';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import * as FormUtils from '@app/lib/forms';
import {ChannelTypes, MessagePreviewContext, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {publicThreadTypeFor, THREAD_NAME_MAX_LENGTH} from '@fluxer/constants/src/ThreadConstants';
import type {MessageStickerItem} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {useLingui} from '@lingui/react/macro';
import {ChatsIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef, useState} from 'react';

function useCooldownSeconds(until: number): number {
	const [now, setNow] = useState(() => Date.now());
	useEffect(() => {
		if (until <= Date.now()) return;
		const timer = window.setInterval(() => setNow(Date.now()), 1000);
		return () => window.clearInterval(timer);
	}, [until]);
	return Math.max(0, Math.ceil((until - now) / 1000));
}

function resolveStarterArgs(
	stickersOrTts?: Array<MessageStickerItem> | boolean,
	favoriteMemeIdOrStickers?: string | Array<MessageStickerItem>,
	maybeFavoriteMemeId?: string,
): {stickers: Array<MessageStickerItem>; favoriteMemeId?: string} {
	if (typeof stickersOrTts === 'boolean') {
		return {
			stickers: Array.isArray(favoriteMemeIdOrStickers) ? favoriteMemeIdOrStickers : [],
			favoriteMemeId: maybeFavoriteMemeId,
		};
	}
	return {
		stickers: stickersOrTts ?? [],
		favoriteMemeId: typeof favoriteMemeIdOrStickers === 'string' ? favoriteMemeIdOrStickers : undefined,
	};
}

interface ThreadCreatePaneProps {
	parent: Channel;
	messageId: string | null;
}

export const ThreadCreatePane = observer(({parent, messageId}: ThreadCreatePaneProps) => {
	const {i18n} = useLingui();
	const [name, setName] = useState('');
	const [nameError, setNameError] = useState<string | undefined>(undefined);
	const [isPrivate, setIsPrivate] = useState(false);
	const nameRef = useRef(name);
	nameRef.current = name;
	const sourceMessage = messageId ? Messages.getMessage(parent.id, messageId) : undefined;
	const draftKey = `thread-create:${parent.id}`;
	const draft = Drafts.getDraft(draftKey);
	const draftSegments = Drafts.getDraftSegments(draftKey);
	const canPrivate =
		messageId == null && parent.type === ChannelTypes.GUILD_TEXT && canCreateThreadIn(parent, 'private');
	const canPublic = canCreateThreadIn(parent, messageId ? 'from_message' : 'public');
	const bypassesSlowmode = Permission.can(Permissions.BYPASS_SLOWMODE, parent);
	const cooldownSeconds = useCooldownSeconds(bypassesSlowmode ? 0 : ThreadPanel.getCreateCooldownUntil(parent.id));
	const privateSelected = canPrivate && (isPrivate || !canPublic);
	const disabled = cooldownSeconds > 0 || (!canPublic && !canPrivate);
	const handleSubmit: SendMessageFunction = useCallback(
		(content, hasAttachments, stickersOrTts, favoriteMemeIdOrStickers, maybeFavoriteMemeId) => {
			const threadName = (nameRef.current.trim() || sourceMessage?.content.trim() || '').slice(
				0,
				THREAD_NAME_MAX_LENGTH,
			);
			if (!threadName) {
				setNameError(i18n._(D.THREAD_NAME_REQUIRED_DESCRIPTOR));
				return false;
			}
			const {stickers, favoriteMemeId} = resolveStarterArgs(
				stickersOrTts,
				favoriteMemeIdOrStickers,
				maybeFavoriteMemeId,
			);
			const nonce = SnowflakeUtils.fromTimestamp(Date.now());
			const attachments = MessageSubmitUtils.createUploadingAttachments(
				MessageSubmitUtils.claimMessageAttachments(draftKey, nonce, content),
				{formatMultipleFileLabel: (count) => formatUploadingAttachmentSummary(i18n, count)},
			);
			const create = messageId
				? ThreadCommands.createThreadFromMessage(parent, messageId, {name: threadName})
				: ThreadCommands.createThread(parent, {
						name: threadName,
						type: privateSelected ? ChannelTypes.PRIVATE_THREAD : publicThreadTypeFor(parent.type),
					});
			void create
				.then((thread) => {
					if (!thread) return;
					ThreadPanel.startCreateCooldown(parent.id, parent.rateLimitPerUser);
					setName('');
					if (content.trim() || attachments.length > 0 || stickers.length > 0 || favoriteMemeId) {
						sendThreadStarter(thread, {content, nonce, attachments, hasAttachments, stickers, favoriteMemeId});
					}
					openThread(thread);
				})
				.catch((error) => {
					if (content && !Drafts.getDraft(draftKey)) DraftCommands.createDraft(draftKey, content);
					CloudUpload.restoreAttachmentsToTextarea(nonce);
					ToastCommands.createToast({
						type: 'error',
						children: i18n._(D.THREAD_CREATE_FAILED_DESCRIPTOR, {detail: FormUtils.extractErrorMessage(i18n, error)}),
					});
				});
			return true;
		},
		[draftKey, i18n, messageId, parent, privateSelected, sourceMessage],
	);
	return (
		<div className={styles.pane} data-flx="threads.thread-create-pane.pane">
			<div className={styles.body} data-flx="threads.thread-create-pane.body">
				<div className={styles.icon} aria-hidden data-flx="threads.thread-create-pane.icon">
					<ChatsIcon className={styles.iconGlyph} data-flx="threads.thread-create-pane.icon-glyph" />
				</div>
				<Input
					value={name}
					onChange={(event) => {
						setName(event.target.value);
						setNameError(undefined);
					}}
					label={i18n._(D.THREAD_NAME_DESCRIPTOR)}
					placeholder={sourceMessage?.content.slice(0, THREAD_NAME_MAX_LENGTH) || i18n._(D.NEW_THREAD_DESCRIPTOR)}
					maxLength={THREAD_NAME_MAX_LENGTH}
					autoFocus
					autoComplete="off"
					error={nameError}
					data-flx="threads.thread-create-pane.input.name"
				/>
				{canPrivate && canPublic && (
					<div className={styles.privateToggle} data-flx="threads.thread-create-pane.private-toggle">
						<Switch
							label={i18n._(D.PRIVATE_THREAD_DESCRIPTOR)}
							description={i18n._(D.PRIVATE_THREAD_HINT_DESCRIPTOR)}
							value={isPrivate}
							onChange={setIsPrivate}
							data-flx="threads.thread-create-pane.switch.private"
						/>
					</div>
				)}
				{sourceMessage && (
					<div className={styles.sourcePreview} data-flx="threads.thread-create-pane.source-preview">
						<MessageComponent
							message={sourceMessage}
							channel={parent}
							previewContext={MessagePreviewContext.LIST_POPOUT}
							data-flx="threads.thread-create-pane.message-component"
						/>
					</div>
				)}
			</div>
			{cooldownSeconds > 0 && (
				<div className={styles.cooldown} role="status" data-flx="threads.thread-create-pane.cooldown">
					{i18n._(D.SLOWMODE_COOLDOWN_DESCRIPTOR, {seconds: cooldownSeconds})}
				</div>
			)}
			<LexicalChannelTextareaContent
				channel={parent}
				draft={draft}
				draftSegments={draftSegments}
				disabled={disabled}
				typingEnabled={false}
				onSubmit={handleSubmit}
				enableReply={false}
				enableEditLast={false}
				enableSlashCommands={false}
				draftChannelId={draftKey}
				placeholder={i18n._(D.START_CONVERSATION_PLACEHOLDER_DESCRIPTOR)}
				data-flx="threads.thread-create-pane.lexical-channel-textarea-content"
			/>
		</div>
	);
});
