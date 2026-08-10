// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/dialogs/ConfirmModal.module.css';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {useElementOverflow} from '@app/features/app/hooks/useTextOverflow';
import {Message} from '@app/features/channel/components/ChannelMessage';
import Channels from '@app/features/channel/state/Channels';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {SHIFT_KEY_LABEL} from '@app/features/input/utils/KeyboardUtils';
import {Message as MessageModel} from '@app/features/messaging/models/MessagingMessage';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import type {ModalProps} from '@app/features/ui/utils/ModalUtils';
import {MessagePreviewContext} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useMemo, useRef, useState} from 'react';

const PRO_TIP_LABEL_DESCRIPTOR = msg({
	message: 'Pro tip:',
	comment:
		'Bolded green prefix shown before the shift-bypass pro tip in confirmation modals. Includes the trailing colon.',
});
const HOLD_SHIFT_TO_SKIP_CONFIRMATION_BODY_DESCRIPTOR = msg({
	message: 'Hold {keyboardShortcut} while choosing this action to skip this confirmation.',
	comment:
		'Body of the reusable pro tip shown in confirmation modals for actions that can be performed immediately by holding Shift when choosing the action. {keyboardShortcut} is the Shift key label.',
});

interface ConfirmModalCheckboxProps {
	checked?: boolean;
	onChange?: (checked: boolean) => void;
}

interface ConfirmModalSwitchProps {
	value?: boolean;
	onChange?: (checked: boolean) => void;
}

interface ConfirmModalCommonProps {
	title: React.ReactNode;
	description: React.ReactNode;
	message?: MessageModel;
	secondaryText?: React.ReactNode | false;
	size?: ModalProps['size'];
	onSecondary?: (checkboxChecked?: boolean) => void;
	checkboxContent?: React.ReactElement<ConfirmModalCheckboxProps>;
	toggleSwitchContent?: React.ReactElement<ConfirmModalSwitchProps>;
	hideCloseButton?: boolean;
	showShiftBypassConfirmationTip?: boolean;
	disableAutoDismiss?: boolean;
}

type ConfirmModalPrimaryVariant = 'primary' | 'danger';
type ConfirmModalProps =
	| (ConfirmModalCommonProps & {
			primaryText: React.ReactNode;
			primaryVariant?: ConfirmModalPrimaryVariant;
			onPrimary: (checkboxChecked?: boolean) => Promise<void> | void;
	  })
	| (ConfirmModalCommonProps & {
			primaryText?: never;
			primaryVariant?: never;
			onPrimary?: never;
	  });

export const ConfirmModal = observer(
	({
		title,
		description,
		message,
		primaryText,
		primaryVariant = 'primary',
		secondaryText,
		size = 'small',
		onPrimary,
		onSecondary,
		checkboxContent,
		toggleSwitchContent,
		hideCloseButton,
		showShiftBypassConfirmationTip = false,
		disableAutoDismiss = false,
	}: ConfirmModalProps) => {
		const {i18n} = useLingui();
		const [submitting, setSubmitting] = useState(false);
		const [checkboxChecked, setCheckboxChecked] = useState(false);
		const [messagePreviewElement, setMessagePreviewElement] = useState<HTMLDivElement | null>(null);
		const initialFocusRef = useRef<HTMLButtonElement | null>(null);
		const isMessagePreviewOverflowing = useElementOverflow(messagePreviewElement, 'vertical');
		const previewBehaviorOverrides = useMemo(
			() => ({
				isEditing: false,
				isHighlight: false,
				disableContextMenu: true,
				disableDoubleClick: true,
				disableContextMenuTracking: true,
				contextMenuOpen: false,
			}),
			[],
		);
		const messageSnapshot = useMemo(() => {
			if (!message) return undefined;
			return new MessageModel(message.toJSON(), {
				skipUserCache: true,
				missingReactions: 'preserve',
				skipReactionHydration: true,
				instanceId: message.instanceId,
			});
		}, [message?.id]);
		const messageSnapshotChannel = messageSnapshot ? Channels.getChannel(messageSnapshot.channelId) : null;
		const handlePrimaryClick = useCallback(async () => {
			if (!onPrimary) {
				return;
			}
			const selfKey = ModalCommands.getTopModalKey();
			setSubmitting(true);
			try {
				await onPrimary(checkboxChecked);
				if (!disableAutoDismiss) {
					if (selfKey != null) {
						ModalCommands.popWithKey(selfKey);
					} else {
						ModalCommands.pop();
					}
				}
			} finally {
				setSubmitting(false);
			}
		}, [onPrimary, checkboxChecked, disableAutoDismiss]);
		const handleSecondaryClick = useCallback(() => {
			const selfKey = ModalCommands.getTopModalKey();
			if (onSecondary) {
				onSecondary(checkboxChecked);
			}
			if (selfKey != null) {
				ModalCommands.popWithKey(selfKey);
			} else {
				ModalCommands.pop();
			}
		}, [onSecondary, checkboxChecked]);
		return (
			<Modal.Root size={size} initialFocusRef={initialFocusRef} centered data-flx="app.confirm-modal.modal-root">
				<Modal.Header title={title} hideCloseButton={hideCloseButton} data-flx="app.confirm-modal.modal-header" />
				<Modal.Content data-flx="app.confirm-modal.modal-content">
					<Modal.ContentLayout data-flx="app.confirm-modal.modal-content-layout">
						<Modal.Description data-flx="app.confirm-modal.modal-description">{description}</Modal.Description>
						{React.isValidElement(checkboxContent) &&
							React.cloneElement(checkboxContent, {
								checked: checkboxChecked,
								onChange: (value: boolean) => setCheckboxChecked(value),
							})}
						{React.isValidElement(toggleSwitchContent) &&
							React.cloneElement(toggleSwitchContent, {
								value: checkboxChecked,
								onChange: (value: boolean) => setCheckboxChecked(value),
							})}
						{messageSnapshot && messageSnapshotChannel && (
							<div
								ref={setMessagePreviewElement}
								className={clsx(styles.messagePreview, isMessagePreviewOverflowing && styles.messagePreviewOverflowing)}
								data-flx="app.confirm-modal.message-preview"
							>
								<Message
									channel={messageSnapshotChannel}
									message={messageSnapshot}
									previewContext={MessagePreviewContext.LIST_POPOUT}
									removeTopSpacing={true}
									behaviorOverrides={previewBehaviorOverrides}
									data-flx="app.confirm-modal.message"
								/>
							</div>
						)}
						{showShiftBypassConfirmationTip && (
							<p className={styles.shiftBypassTip} data-flx="app.confirm-modal.shift-bypass-tip">
								<span className={styles.shiftBypassTipLabel} data-flx="app.confirm-modal.shift-bypass-tip.label">
									{i18n._(PRO_TIP_LABEL_DESCRIPTOR)}
								</span>{' '}
								{i18n._(HOLD_SHIFT_TO_SKIP_CONFIRMATION_BODY_DESCRIPTOR, {keyboardShortcut: SHIFT_KEY_LABEL})}
							</p>
						)}
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="app.confirm-modal.footer">
					{secondaryText !== false && (
						<Button
							onClick={handleSecondaryClick}
							variant="secondary"
							data-flx="app.confirm-modal.button.secondary-click"
						>
							{secondaryText ?? i18n._(CANCEL_DESCRIPTOR)}
						</Button>
					)}
					{onPrimary && primaryText && (
						<Button
							onClick={handlePrimaryClick}
							submitting={submitting}
							variant={primaryVariant}
							ref={initialFocusRef}
							data-flx="app.confirm-modal.button.primary-click"
						>
							{primaryText}
						</Button>
					)}
				</Modal.Footer>
			</Modal.Root>
		);
	},
);
