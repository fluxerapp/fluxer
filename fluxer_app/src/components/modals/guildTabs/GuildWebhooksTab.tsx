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
import {RobotIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as WebhookActionCreators from '~/actions/WebhookActionCreators';
import {Permissions, TEXT_BASED_CHANNEL_TYPES} from '~/Constants';
import {StatusSlate} from '~/components/modals/shared/StatusSlate';
import {Spinner} from '~/components/uikit/Spinner';
import {WebhookListItem} from '~/components/webhooks/WebhookListItem';
import {useWebhookUpdates} from '~/hooks/useWebhookUpdates';
import type {WebhookRecord} from '~/records/WebhookRecord';
import ChannelStore from '~/stores/ChannelStore';
import PermissionStore from '~/stores/PermissionStore';
import WebhookStore from '~/stores/WebhookStore';
import styles from './GuildWebhooksTab.module.css';

const GUILD_WEBHOOKS_TAB_ID = 'webhooks';

const GuildWebhooksTab: React.FC<{guildId: string}> = observer(({guildId}) => {
	const {t} = useLingui();
	const canManageWebhooks = PermissionStore.can(Permissions.MANAGE_WEBHOOKS, {guildId});

	const fetchStatus = WebhookStore.getGuildFetchStatus(guildId);
	const webhooks = WebhookStore.getGuildWebhooks(guildId);
	const guildChannels = ChannelStore.getGuildChannels(guildId);

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

	const channelNameMap = React.useMemo(() => {
		const map = new Map<string, string>();
		for (const ch of guildChannels) map.set(ch.id, ch.name ?? t`Unknown channel`);
		return map;
	}, [guildChannels]);

	const sortedWebhooks = React.useMemo(() => {
		return [...webhooks].sort((a, b) => {
			const channelA = channelNameMap.get(a.channelId) ?? '';
			const channelB = channelNameMap.get(b.channelId) ?? '';
			if (channelA.localeCompare(channelB) !== 0) {
				return channelA.localeCompare(channelB, undefined, {numeric: true, sensitivity: 'base'});
			}
			return a.name.localeCompare(b.name, undefined, {numeric: true, sensitivity: 'base'});
		});
	}, [webhooks, channelNameMap]);

	React.useEffect(() => {
		if (!canManageWebhooks) return;
		if (fetchStatus === 'idle') {
			void WebhookActionCreators.fetchGuildWebhooks?.(guildId);
		}
	}, [fetchStatus, guildId, canManageWebhooks]);

	const {handleUpdate, formVersion} = useWebhookUpdates({
		tabId: GUILD_WEBHOOKS_TAB_ID,
		canManage: canManageWebhooks,
		originals: webhooks,
	});

	const header = (
		<div className={styles.header}>
			<h2 className={styles.title}>
				<Trans>Webhooks</Trans>
			</h2>
			<p className={styles.subtitle}>
				<Trans>View and manage every webhook configured across your community.</Trans>
			</p>
		</div>
	);

	return (
		<div className={styles.container}>
			{header}

			{!canManageWebhooks && (
				<div className={styles.notice}>
					<Trans>You need the Manage Webhooks permission to view and edit webhooks for this community.</Trans>
				</div>
			)}

			{canManageWebhooks && (
				<div className={styles.infoBox}>
					<Trans>
						To create a webhook, open the channel's settings and use the <strong>Webhooks</strong> tab. You can still
						edit and organize all existing webhooks here.
					</Trans>
				</div>
			)}

			{fetchStatus === 'pending' && (
				<div className={styles.spinnerContainer}>
					<Spinner />
				</div>
			)}

			{fetchStatus === 'error' && (
				<StatusSlate
					Icon={WarningCircleIcon}
					title={t`Failed to load webhooks`}
					description={t`There was an error loading the webhooks. Please try again.`}
					actions={[
						{
							text: t`Retry`,
							onClick: () => WebhookActionCreators.fetchGuildWebhooks?.(guildId),
							variant: 'primary',
						},
					]}
					fullHeight={true}
				/>
			)}

			{fetchStatus === 'success' && sortedWebhooks.length > 0 && (
				<div className={styles.webhookList}>
					{sortedWebhooks.map((webhook: WebhookRecord) => (
						<WebhookListItem
							key={webhook.id}
							webhook={webhook}
							channelName={channelNameMap.get(webhook.channelId) ?? undefined}
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

			{fetchStatus === 'success' && sortedWebhooks.length === 0 && (
				<StatusSlate
					Icon={RobotIcon}
					title={<Trans>No webhooks</Trans>}
					description={
						<Trans>
							This community doesn't have any webhooks yet. Go to a channel's settings and use the Webhooks tab to
							create one.
						</Trans>
					}
					fullHeight={true}
				/>
			)}
		</div>
	);
});

export default GuildWebhooksTab;
