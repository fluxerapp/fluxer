// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/ThreadCreationPanel.module.css';
import * as ThreadCommands from '@app/features/channel/commands/ThreadCommands';
import Threads from '@app/features/channel/state/Threads';
import ThreadCreation from '@app/features/channel/state/ThreadCreation';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {http} from '@app/features/platform/transport/RestTransport';
import {ThreadIcon} from '@app/features/ui/components/icons/ThreadIcon';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {WarningCircleIcon, XIcon} from '@phosphor-icons/react';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useRef, useState} from 'react';

const NEW_THREAD_DESCRIPTOR = msg({
	message: 'New Thread',
	comment: 'Header title in the thread creation panel.',
});
const CLOSE_DESCRIPTOR = msg({
	message: 'Close',
	comment: 'Accessible label on the close button in the thread creation panel.',
});
const THREAD_NAME_LABEL_DESCRIPTOR = msg({
	message: 'Thread name (optional)',
	comment: 'Label for the thread name input.',
});
const STARTER_MESSAGE_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'Enter a message to start the conversation!',
	comment: 'Placeholder for the starter message textarea.',
});
const STARTER_REQUIRED_DESCRIPTOR = msg({
	message: 'Starter message is required',
	comment: 'Validation hint when no message has been typed.',
});
const CONFIRM_EXIT_TITLE_DESCRIPTOR = msg({
	message: 'Are you sure you want to go?',
	comment: 'Exit confirmation dialog title.',
});
const CONFIRM_EXIT_BODY_DESCRIPTOR = msg({
	message: 'Any changes you made will be lost if you exit now.',
	comment: 'Exit confirmation dialog body.',
});
const BACK_TO_EDITING_DESCRIPTOR = msg({
	message: 'Back to editing',
	comment: 'Cancel button in the exit confirmation dialog.',
});
const EXIT_DESCRIPTOR = msg({
	message: 'Exit',
	comment: 'Confirm exit button in the exit confirmation dialog.',
});

interface ThreadCreationPanelProps {
	channelId: string;
	guildId: string;
	sourceMessageId?: string;
	sourceMessagePreview?: string;
	sourceMessageAuthor?: string;
	onClose: () => void;
}

export const ThreadCreationPanel = observer(({
	channelId,
	guildId,
	sourceMessageId,
	sourceMessagePreview,
	sourceMessageAuthor,
	onClose,
}: ThreadCreationPanelProps) => {
	const {i18n} = useLingui();
	const [threadName, setThreadName] = useState('');
	const [message, setMessage] = useState('');
	const [submitting, setSubmitting] = useState(false);
	const [showExitConfirm, setShowExitConfirm] = useState(false);
	const textareaRef = useRef<HTMLTextAreaElement>(null);

	const isDirty = threadName.trim().length > 0 || message.trim().length > 0;

	const handleCloseAttempt = useCallback(() => {
		if (isDirty) {
			setShowExitConfirm(true);
		} else {
			onClose();
		}
	}, [isDirty, onClose]);

	const submit = useCallback(async () => {
		const trimmed = message.trim();
		if (!trimmed || submitting) return;
		setSubmitting(true);
		try {
			const resolvedName = threadName.trim() || (sourceMessagePreview ? sourceMessagePreview.slice(0, 36) : 'New Thread');
			const thread = await ThreadCommands.create(channelId, {
				name: resolvedName,
				source_message_id: sourceMessageId,
			});
			Threads.handleThreadCreate(thread);
			Threads.handleThreadMemberAdd({threadId: thread.id});
			await http.post(`/channels/${thread.id}/messages`, {body: {content: trimmed}});
			if (sourceMessageId) {
				const {default: Messages} = await import('@app/features/messaging/state/MessagingMessages');
				const existing = Messages.getMessage(channelId, sourceMessageId);
				if (existing) {
					Messages.handleMessageUpdate({
						message: {
							...existing.toJSON(),
							thread_id: thread.id,
							thread_name: thread.name ?? resolvedName,
						},
					});
				}
			}
			ThreadCreation.setCreated(thread.id);
			NavigationCommands.selectThread(guildId, channelId, thread.id);
		} finally {
			setSubmitting(false);
		}
	}, [channelId, guildId, sourceMessageId, threadName, message, submitting]);

	const handleKeyDown = useCallback(
		(e: React.KeyboardEvent<HTMLTextAreaElement>) => {
			if (e.key !== 'Enter' || e.shiftKey) return;
			e.preventDefault();
			void submit();
		},
		[submit],
	);

	const hasMessage = message.trim().length > 0;

	return (
		<div className={styles.panel} data-flx="channel.thread-creation-panel.panel">
			<div className={styles.header} data-flx="channel.thread-creation-panel.header">
				<ThreadIcon size={16} className={styles.headerIcon} data-flx="channel.thread-creation-panel.header-icon" />
				<span className={styles.headerTitle} data-flx="channel.thread-creation-panel.header-title">
					{i18n._(NEW_THREAD_DESCRIPTOR)}
				</span>
				<FocusRing data-flx="channel.thread-creation-panel.focus-ring">
					<button
						type="button"
						className={styles.closeButton}
						aria-label={i18n._(CLOSE_DESCRIPTOR)}
						onClick={handleCloseAttempt}
						data-flx="channel.thread-creation-panel.close-button.click"
					>
						<XIcon size={18} />
					</button>
				</FocusRing>
			</div>

			<div className={styles.body} data-flx="channel.thread-creation-panel.body">
				<div className={styles.glyph} aria-hidden="true" data-flx="channel.thread-creation-panel.glyph">
					<ThreadIcon size={28} />
				</div>

				<div className={styles.field} data-flx="channel.thread-creation-panel.field-name">
					<label className={styles.fieldLabel} data-flx="channel.thread-creation-panel.field-label">
						{i18n._(THREAD_NAME_LABEL_DESCRIPTOR)}
					</label>
					<input
						className={styles.nameInput}
						type="text"
						maxLength={36}
						value={threadName}
						placeholder={sourceMessagePreview ? sourceMessagePreview.slice(0, 36) : undefined}
						onChange={(e) => setThreadName(e.currentTarget.value)}
						autoFocus
						data-flx="channel.thread-creation-panel.name-input"
					/>
				</div>

				{sourceMessagePreview && (
					<div className={styles.sourceRow} data-flx="channel.thread-creation-panel.source-row">
						<div className={styles.sourceAvatar} aria-hidden="true" data-flx="channel.thread-creation-panel.source-avatar">
							{(sourceMessageAuthor ?? '?')[0].toUpperCase()}
						</div>
						<div className={styles.sourceContent} data-flx="channel.thread-creation-panel.source-content">
							{sourceMessageAuthor && (
								<span className={styles.sourceAuthor} data-flx="channel.thread-creation-panel.source-author">
									{sourceMessageAuthor}
								</span>
							)}
							<span className={styles.sourceText} data-flx="channel.thread-creation-panel.source-text">
								{sourceMessagePreview}
							</span>
						</div>
					</div>
				)}
			</div>

			<div className={styles.footer} data-flx="channel.thread-creation-panel.footer">
				{!hasMessage && (
					<div className={styles.validation} data-flx="channel.thread-creation-panel.validation">
						<WarningCircleIcon size={13} aria-hidden="true" />
						{i18n._(STARTER_REQUIRED_DESCRIPTOR)}
					</div>
				)}
				<div
					className={clsx(styles.textareaBox, !hasMessage ? styles.textareaBoxError : undefined)}
					data-flx="channel.thread-creation-panel.textarea-box"
				>
					<textarea
						ref={textareaRef}
						className={styles.textarea}
						placeholder={i18n._(STARTER_MESSAGE_PLACEHOLDER_DESCRIPTOR)}
						value={message}
						onChange={(e) => setMessage(e.currentTarget.value)}
						onKeyDown={handleKeyDown}
						disabled={submitting}
						rows={1}
						data-flx="channel.thread-creation-panel.textarea"
					/>
				</div>
			</div>

			{showExitConfirm && (
				<div className={styles.overlay} data-flx="channel.thread-creation-panel.exit-overlay">
					<div className={styles.confirmBox} data-flx="channel.thread-creation-panel.confirm-box">
						<p className={styles.confirmTitle} data-flx="channel.thread-creation-panel.confirm-title">
							{i18n._(CONFIRM_EXIT_TITLE_DESCRIPTOR)}
						</p>
						<p className={styles.confirmBody} data-flx="channel.thread-creation-panel.confirm-body">
							{i18n._(CONFIRM_EXIT_BODY_DESCRIPTOR)}
						</p>
						<div className={styles.confirmActions} data-flx="channel.thread-creation-panel.confirm-actions">
							<button
								type="button"
								className={styles.btnSecondary}
								onClick={() => setShowExitConfirm(false)}
								data-flx="channel.thread-creation-panel.confirm-back.click"
							>
								{i18n._(BACK_TO_EDITING_DESCRIPTOR)}
							</button>
							<button
								type="button"
								className={styles.btnDanger}
								onClick={onClose}
								data-flx="channel.thread-creation-panel.confirm-exit.click"
							>
								{i18n._(EXIT_DESCRIPTOR)}
							</button>
						</div>
					</div>
				</div>
			)}
		</div>
	);
});
