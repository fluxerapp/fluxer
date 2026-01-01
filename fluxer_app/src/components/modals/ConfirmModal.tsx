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
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {MessagePreviewContext} from '~/Constants';
import {Message} from '~/components/channel/Message';
import styles from '~/components/modals/ConfirmModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {MessageRecord} from '~/records/MessageRecord';
import ChannelStore from '~/stores/ChannelStore';

type ConfirmModalCheckboxProps = {
	checked?: boolean;
	onChange?: (checked: boolean) => void;
};

type ConfirmModalPrimaryVariant = 'primary' | 'danger-primary';

type ConfirmModalProps =
	| {
			title: React.ReactNode;
			description: React.ReactNode;
			message?: MessageRecord;
			primaryText: React.ReactNode;
			primaryVariant?: ConfirmModalPrimaryVariant;
			secondaryText?: React.ReactNode | false;
			size?: Modal.ModalProps['size'];
			onPrimary: (checkboxChecked?: boolean) => Promise<void> | void;
			onSecondary?: (checkboxChecked?: boolean) => void;
			checkboxContent?: React.ReactElement<ConfirmModalCheckboxProps>;
	  }
	| {
			title: React.ReactNode;
			description: React.ReactNode;
			message?: MessageRecord;
			primaryText?: never;
			primaryVariant?: never;
			secondaryText?: React.ReactNode | false;
			size?: Modal.ModalProps['size'];
			onPrimary?: never;
			onSecondary?: (checkboxChecked?: boolean) => void;
			checkboxContent?: React.ReactElement<ConfirmModalCheckboxProps>;
	  };

export const ConfirmModal = observer(
	({
		title,
		description,
		message,
		primaryText,
		primaryVariant = 'danger-primary',
		secondaryText,
		size = 'small',
		onPrimary,
		onSecondary,
		checkboxContent,
	}: ConfirmModalProps) => {
		const {t} = useLingui();
		const [submitting, setSubmitting] = React.useState(false);
		const [checkboxChecked, setCheckboxChecked] = React.useState(false);
		const initialFocusRef = React.useRef<HTMLButtonElement | null>(null);
		const previewBehaviorOverrides = React.useMemo(
			() => ({
				isEditing: false,
				isHighlight: false,
				disableContextMenu: true,
				disableContextMenuTracking: true,
				contextMenuOpen: false,
			}),
			[],
		);

		const messageSnapshot = React.useMemo(() => {
			if (!message) return undefined;
			return new MessageRecord(message.toJSON());
		}, [message?.id]);

		const handlePrimaryClick = React.useCallback(async () => {
			if (!onPrimary) {
				return;
			}
			setSubmitting(true);
			try {
				await onPrimary(checkboxChecked);
				ModalActionCreators.pop();
			} finally {
				setSubmitting(false);
			}
		}, [onPrimary, checkboxChecked]);

		const handleSecondaryClick = React.useCallback(() => {
			if (onSecondary) {
				onSecondary(checkboxChecked);
			}
			ModalActionCreators.pop();
		}, [onSecondary, checkboxChecked]);

		return (
			<Modal.Root size={size} initialFocusRef={initialFocusRef} centered>
				<Modal.Header title={title} />
				<Modal.Content className={styles.content}>
					<div className={styles.descriptionText}>{description}</div>
					{React.isValidElement(checkboxContent) && (
						<div style={{marginTop: '16px'}}>
							{React.cloneElement(checkboxContent, {
								checked: checkboxChecked,
								onChange: (value: boolean) => setCheckboxChecked(value),
							})}
						</div>
					)}
					{messageSnapshot && (
						<div className={styles.messagePreview}>
							<Message
								channel={ChannelStore.getChannel(messageSnapshot.channelId)!}
								message={messageSnapshot}
								previewContext={MessagePreviewContext.LIST_POPOUT}
								removeTopSpacing={true}
								behaviorOverrides={previewBehaviorOverrides}
							/>
						</div>
					)}
				</Modal.Content>
				<Modal.Footer className={styles.footer}>
					{secondaryText !== false && (
						<Button onClick={handleSecondaryClick} variant="secondary">
							{secondaryText ?? t`Cancel`}
						</Button>
					)}
					{onPrimary && primaryText && (
						<Button onClick={handlePrimaryClick} submitting={submitting} variant={primaryVariant} ref={initialFocusRef}>
							{primaryText}
						</Button>
					)}
				</Modal.Footer>
			</Modal.Root>
		);
	},
);
