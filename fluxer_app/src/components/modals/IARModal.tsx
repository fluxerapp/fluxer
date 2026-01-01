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

import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as IARActionCreators from '~/actions/IARActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {MessagePreviewContext} from '~/Constants';
import {Message} from '~/components/channel/Message';
import {Textarea} from '~/components/form/Input';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {RadioGroup} from '~/components/uikit/RadioGroup/RadioGroup';
import {
	getGuildViolationCategories,
	getMessageViolationCategories,
	getUserViolationCategories,
} from '~/constants/IARConstants';
import type {GuildRecord} from '~/records/GuildRecord';
import type {MessageRecord} from '~/records/MessageRecord';
import type {UserRecord} from '~/records/UserRecord';
import ChannelStore from '~/stores/ChannelStore';
import styles from './IARModal.module.css';

export type IARContext =
	| {
			type: 'message';
			message: MessageRecord;
	  }
	| {
			type: 'user';
			user: UserRecord;
			guildId?: string;
	  }
	| {
			type: 'guild';
			guild: GuildRecord;
	  };

interface IARModalProps {
	context: IARContext;
}

export const IARModal: React.FC<IARModalProps> = observer(({context}) => {
	const {t, i18n} = useLingui();
	const [selectedCategory, setSelectedCategory] = React.useState<string>('');
	const [additionalInfo, setAdditionalInfo] = React.useState('');
	const [submitting, setSubmitting] = React.useState(false);

	const categories = React.useMemo(() => {
		switch (context.type) {
			case 'message':
				return getMessageViolationCategories(i18n);
			case 'user':
				return getUserViolationCategories(i18n);
			case 'guild':
				return getGuildViolationCategories(i18n);
		}
	}, [context.type, i18n]);

	const title = React.useMemo(() => {
		switch (context.type) {
			case 'message':
				return t`Report Message`;
			case 'user':
				return t`Report User`;
			case 'guild':
				return t`Report Community`;
		}
	}, [context.type]);

	const handleCategoryChange = React.useCallback((value: string) => {
		setSelectedCategory(value);
	}, []);

	const handleAdditionalInfoChange = React.useCallback((e: React.ChangeEvent<HTMLTextAreaElement>) => {
		setAdditionalInfo(e.target.value);
	}, []);

	const handleSubmit = React.useCallback(async () => {
		if (!selectedCategory) {
			ToastActionCreators.createToast({
				type: 'error',
				children: <Trans>Please select a violation category</Trans>,
			});
			return;
		}

		setSubmitting(true);
		try {
			switch (context.type) {
				case 'message':
					await IARActionCreators.reportMessage(
						context.message.channelId,
						context.message.id,
						selectedCategory,
						additionalInfo,
					);
					break;
				case 'user':
					await IARActionCreators.reportUser(context.user.id, selectedCategory, additionalInfo, context.guildId);
					break;
				case 'guild':
					await IARActionCreators.reportGuild(context.guild.id, selectedCategory, additionalInfo);
					break;
			}

			ToastActionCreators.createToast({
				type: 'success',
				children: <Trans>Report submitted successfully. Our Safety Team will review it shortly.</Trans>,
			});
			ModalActionCreators.pop();
		} catch (error) {
			console.error('Failed to submit report:', error);
			ToastActionCreators.createToast({
				type: 'error',
				children: <Trans>Failed to submit report. Please try again.</Trans>,
			});
		} finally {
			setSubmitting(false);
		}
	}, [context, selectedCategory, additionalInfo]);

	const renderPreview = () => {
		if (context.type === 'message') {
			const channel = ChannelStore.getChannel(context.message.channelId);
			if (!channel) return null;

			return (
				<div className={styles.preview}>
					<Message
						channel={channel}
						message={context.message}
						previewContext={MessagePreviewContext.LIST_POPOUT}
						removeTopSpacing={true}
					/>
				</div>
			);
		}

		if (context.type === 'user') {
			return (
				<div className={styles.userPreview}>
					<div className={styles.userInfo}>
						<span className={styles.username}>{context.user.username}</span>
						<span className={styles.discriminator}>#{context.user.discriminator?.toString().padStart(4, '0')}</span>
					</div>
				</div>
			);
		}

		if (context.type === 'guild') {
			return (
				<div className={styles.guildPreview}>
					<span className={styles.guildName}>{context.guild.name}</span>
				</div>
			);
		}

		return null;
	};

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={title} />
			<Modal.Content>
				<div className={styles.container}>
					<p className={styles.description}>
						<Trans>
							Thank you for helping keep Fluxer safe. Reports are reviewed by our Safety Team. False reports may result
							in action against your account.
						</Trans>
					</p>
					{renderPreview()}
					<div className={styles.categorySection}>
						<div className={styles.categoryLabel}>{t`Why are you reporting this?`}</div>
						<RadioGroup
							value={selectedCategory || null}
							options={categories}
							onChange={handleCategoryChange}
							disabled={submitting}
							aria-label={t`Violation category selection`}
						/>
					</div>
					<Textarea
						label={t`Additional information (optional)`}
						value={additionalInfo}
						onChange={handleAdditionalInfoChange}
						placeholder={t`Provide any additional context that may help our Safety Team...`}
						minRows={3}
						maxRows={8}
						maxLength={1000}
						showCharacterCount={true}
						disabled={submitting}
					/>
				</div>
			</Modal.Content>
			<Modal.Footer>
				<Button variant="secondary" onClick={() => ModalActionCreators.pop()} disabled={submitting}>
					<Trans>Cancel</Trans>
				</Button>
				<Button variant="danger-primary" onClick={handleSubmit} disabled={!selectedCategory || submitting}>
					<Trans>Submit Report</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
