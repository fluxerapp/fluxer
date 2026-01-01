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
import {RobotIcon, WarningOctagonIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as WebhookActionCreators from '~/actions/WebhookActionCreators';
import {Permissions, TEXT_BASED_CHANNEL_TYPES} from '~/Constants';
import {StatusSlate} from '~/components/modals/shared/StatusSlate';
import {Button} from '~/components/uikit/Button/Button';
import {Spinner} from '~/components/uikit/Spinner';
import {WebhookListItem} from '~/components/webhooks/WebhookListItem';
import {useWebhookUpdates} from '~/hooks/useWebhookUpdates';
import type {WebhookRecord} from '~/records/WebhookRecord';
import ChannelStore from '~/stores/ChannelStore';
import PermissionStore from '~/stores/PermissionStore';
import WebhookStore from '~/stores/WebhookStore';
import {generateRandomWebhookName} from '~/utils/WebhookUtils';
import styles from './ChannelWebhooksTab.module.css';

const CHANNEL_WEBHOOKS_TAB_ID = 'webhooks';

const ChannelWebhooksTab: React.FC<{channelId: string}> = observer(({channelId}) => {
	const {t} = useLingui();
	const channel = ChannelStore.getChannel(channelId);
	const guildId = channel?.guildId ?? null;

	const canManageWebhooks =
		guildId && channel ? PermissionStore.can(Permissions.MANAGE_WEBHOOKS, {channelId, guildId}) : false;

	const fetchStatus = WebhookStore.getChannelFetchStatus(channelId);
	const webhooks = WebhookStore.getChannelWebhooks(channelId);
	const guildChannels = ChannelStore.getGuildChannels(guildId ?? '');

	const [expandedIds, setExpandedIds] = React.useState<Set<string>>(new Set());
	const setExpanded = React.useCallback((id: string, expanded: boolean) => {
		setExpandedIds((prev) => {
			const next = new Set(prev);
			if (expanded) next.add(id);
			else next.delete(id);
			return next;
		});
	}, []);

	const availableChannels = React.useMemo(
		() =>
			guildChannels
				.filter((ch) => TEXT_BASED_CHANNEL_TYPES.has(ch.type))
				.map((ch) => ({id: ch.id, label: ch.name ?? t`Unknown channel`})),
		[guildChannels],
	);

	const refreshWebhooks = React.useCallback(async () => {
		if (!guildId) return;
		try {
			await WebhookActionCreators.fetchChannelWebhooks({guildId, channelId});
		} catch (error) {
			console.error('Failed to refresh webhooks', error);
		}
	}, [guildId, channelId]);

	React.useEffect(() => {
		if (!guildId || !canManageWebhooks) return;
		if (fetchStatus === 'idle') {
			void refreshWebhooks();
		}
	}, [fetchStatus, guildId, channelId, canManageWebhooks, refreshWebhooks]);

	const {handleUpdate, formVersion} = useWebhookUpdates({
		tabId: CHANNEL_WEBHOOKS_TAB_ID,
		canManage: canManageWebhooks,
		originals: webhooks ?? undefined,
	});

	const header = (
		<div>
			<h2 className={styles.header}>
				<Trans>Webhooks</Trans>
			</h2>
			<p className={styles.description}>
				<Trans>Manage incoming webhooks that can post messages into this channel.</Trans>
			</p>
		</div>
	);

	const handleCreateQuick = React.useCallback(async () => {
		if (!canManageWebhooks) return;
		try {
			const name = generateRandomWebhookName();
			await WebhookActionCreators.createWebhook({channelId, name});
			ToastActionCreators.createToast({type: 'success', children: t`Webhook created`});
			void WebhookActionCreators.fetchChannelWebhooks({guildId: guildId!, channelId}).catch(() => {});
		} catch (error) {
			console.error('Failed to create webhook', error);
			ToastActionCreators.createToast({type: 'error', children: t`Failed to create webhook`});
		}
	}, [canManageWebhooks, channelId]);

	if (!channel || !guildId || !TEXT_BASED_CHANNEL_TYPES.has(channel.type)) {
		return (
			<div className={styles.container}>
				{header}
				<div className={styles.messageBox}>
					<Trans>This channel does not support webhooks.</Trans>
				</div>
			</div>
		);
	}

	return (
		<div className={styles.container}>
			{header}

			{!canManageWebhooks && (
				<div className={styles.messageBox}>
					<Trans>You need the Manage Webhooks permission to view and edit webhooks for this channel.</Trans>
				</div>
			)}

			{canManageWebhooks && !(fetchStatus === 'success' && (!webhooks || webhooks.length === 0)) && (
				<div className={styles.buttonContainer}>
					<Button onClick={handleCreateQuick} variant="primary" disabled={fetchStatus === 'pending'} small>
						<Trans>Create Webhook</Trans>
					</Button>
				</div>
			)}

			{fetchStatus === 'pending' && (
				<div className={styles.spinnerContainer}>
					<Spinner />
				</div>
			)}

			{fetchStatus === 'error' && (
				<StatusSlate
					Icon={WarningOctagonIcon}
					title={<Trans>Failed to load webhooks</Trans>}
					description={<Trans>There was an error loading the webhooks for this channel. Please try again.</Trans>}
					actions={[
						{
							text: <Trans>Try Again</Trans>,
							onClick: refreshWebhooks,
							variant: 'primary',
						},
					]}
					fullHeight={true}
				/>
			)}

			{fetchStatus === 'success' && webhooks && webhooks.length > 0 && (
				<div className={styles.webhooksList}>
					{webhooks.map((webhook: WebhookRecord) => (
						<WebhookListItem
							key={webhook.id}
							webhook={webhook}
							onUpdate={handleUpdate}
							onDelete={(webhook) => WebhookActionCreators.deleteWebhook(webhook.id)}
							availableChannels={availableChannels}
							defaultExpanded={false}
							isExpanded={expandedIds.has(webhook.id)}
							onExpandedChange={(open) => setExpanded(webhook.id, open)}
							formVersion={formVersion}
						/>
					))}
				</div>
			)}

			{fetchStatus === 'success' && (!webhooks || webhooks.length === 0) && (
				<StatusSlate
					Icon={RobotIcon}
					title={<Trans>No webhooks</Trans>}
					description={
						<Trans>
							There are no webhooks configured for this channel. Create a webhook to allow external applications to post
							messages.
						</Trans>
					}
					actions={
						canManageWebhooks
							? [
									{
										text: <Trans>Create Webhook</Trans>,
										onClick: handleCreateQuick,
										variant: 'primary',
									},
								]
							: undefined
					}
					fullHeight={true}
				/>
			)}
		</div>
	);
});

export default ChannelWebhooksTab;
