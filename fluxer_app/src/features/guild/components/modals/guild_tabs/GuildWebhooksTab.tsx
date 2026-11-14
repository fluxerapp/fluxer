// SPDX-License-Identifier: AGPL-3.0-or-later

import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import Channels from '@app/features/channel/state/Channels';
import {UNKNOWN_CHANNEL_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import styles from '@app/features/guild/components/modals/guild_tabs/GuildWebhooksTab.module.css';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Permission from '@app/features/permissions/state/Permission';
import {formatPermissionLabel} from '@app/features/permissions/utils/PermissionUtils';
import {Spinner} from '@app/features/ui/components/Spinner';
import {formatChannelSettingsPath} from '@app/features/user/components/settings_utils/ChannelSettingsConstants';
import * as WebhookCommands from '@app/features/webhook/commands/WebhookCommands';
import {FollowedChannelListItem} from '@app/features/webhook/components/FollowedChannelListItem';
import {WebhookListItem} from '@app/features/webhook/components/WebhookListItem';
import {useWebhookUpdates} from '@app/features/webhook/hooks/useWebhookUpdates';
import type {Webhook} from '@app/features/webhook/models/Webhook';
import Webhooks from '@app/features/webhook/state/Webhooks';
import {GUILD_TEXT_BASED_CHANNEL_TYPES, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {RobotIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

const FAILED_TO_LOAD_WEBHOOKS_DESCRIPTOR = msg({
	message: 'Failed to load webhooks',
	comment: 'Error message in the guild webhooks tab.',
});
const THERE_WAS_AN_ERROR_LOADING_THE_WEBHOOKS_PLEASE_DESCRIPTOR = msg({
	message: 'There was an error loading the webhooks. Try again.',
	comment: 'Error message in the guild webhooks tab.',
});
const GUILD_WEBHOOKS_TAB_ID = 'webhooks';
const GuildWebhooksTab: React.FC<{guildId: string}> = observer(({guildId}) => {
	const {i18n} = useLingui();
	const canManageWebhooks = Permission.can(Permissions.MANAGE_WEBHOOKS, {guildId});
	const channelWebhooksSettingsPath = formatChannelSettingsPath(i18n, 'webhooks');
	const manageWebhooksPermissionLabel = formatPermissionLabel(i18n, Permissions.MANAGE_WEBHOOKS);
	const fetchStatus = Webhooks.getGuildFetchStatus(guildId);
	const webhooks = Webhooks.getGuildWebhooks(guildId);
	const guildChannels = Channels.getGuildChannels(guildId);
	const [expandedIds, setExpandedIds] = useState<Set<string>>(new Set());
	const setExpanded = useCallback((id: string, expanded: boolean) => {
		setExpandedIds((prev) => {
			const next = new Set(prev);
			if (expanded) next.add(id);
			else next.delete(id);
			return next;
		});
	}, []);
	const availableChannels = useMemo(
		() =>
			guildChannels
				.filter((ch) => GUILD_TEXT_BASED_CHANNEL_TYPES.has(ch.type))
				.map((ch) => ({id: ch.id, label: ch.name ?? i18n._(UNKNOWN_CHANNEL_DESCRIPTOR)})),
		[guildChannels, i18n.locale],
	);
	const channelNameMap = useMemo(() => {
		const map = new Map<string, string>();
		for (const ch of guildChannels) map.set(ch.id, ch.name ?? i18n._(UNKNOWN_CHANNEL_DESCRIPTOR));
		return map;
	}, [guildChannels, i18n.locale]);
	const sortedWebhooks = useMemo(() => {
		return [...webhooks].sort((a, b) => {
			const channelA = channelNameMap.get(a.channelId) ?? '';
			const channelB = channelNameMap.get(b.channelId) ?? '';
			if (channelA.localeCompare(channelB) !== 0) {
				return channelA.localeCompare(channelB, undefined, {numeric: true, sensitivity: 'base'});
			}
			return a.name.localeCompare(b.name, undefined, {numeric: true, sensitivity: 'base'});
		});
	}, [webhooks, channelNameMap]);
	const incomingWebhooks = useMemo(
		() => sortedWebhooks.filter((webhook) => !webhook.isChannelFollower),
		[sortedWebhooks],
	);
	const followerWebhookGroups = useMemo(() => {
		const groups = new Map<string, Array<Webhook>>();
		for (const webhook of sortedWebhooks) {
			if (!webhook.isChannelFollower) continue;
			const group = groups.get(webhook.channelId);
			if (group) group.push(webhook);
			else groups.set(webhook.channelId, [webhook]);
		}
		return Array.from(groups, ([channelId, items]) => ({
			channelId,
			channelName: channelNameMap.get(channelId) ?? i18n._(UNKNOWN_CHANNEL_DESCRIPTOR),
			items,
		}));
	}, [sortedWebhooks, channelNameMap, i18n.locale]);
	useEffect(() => {
		if (!canManageWebhooks) return;
		if (fetchStatus === 'idle') {
			void WebhookCommands.fetchGuildWebhooks?.(guildId);
		}
	}, [fetchStatus, guildId, canManageWebhooks]);
	const {handleUpdate, formVersion} = useWebhookUpdates({
		tabId: GUILD_WEBHOOKS_TAB_ID,
		canManage: canManageWebhooks,
		originals: webhooks,
	});
	const header = (
		<div className={styles.header} data-flx="guild.guild-tabs.guild-webhooks-tab.header">
			<h2 className={styles.title} data-flx="guild.guild-tabs.guild-webhooks-tab.title">
				<Trans>Webhooks</Trans>
			</h2>
			<p className={styles.subtitle} data-flx="guild.guild-tabs.guild-webhooks-tab.subtitle">
				<Trans>View and manage every webhook configured across your community.</Trans>
			</p>
		</div>
	);
	return (
		<div className={styles.container} data-flx="guild.guild-tabs.guild-webhooks-tab.container">
			{header}
			{!canManageWebhooks && (
				<div className={styles.notice} data-flx="guild.guild-tabs.guild-webhooks-tab.notice">
					<Trans>
						You need the "{manageWebhooksPermissionLabel}" permission to view and edit webhooks for this community.
					</Trans>
				</div>
			)}
			{canManageWebhooks && (
				<div className={styles.infoBox} data-flx="guild.guild-tabs.guild-webhooks-tab.info-box">
					<Trans>
						To create a webhook, open{' '}
						<strong data-flx="guild.guild-tabs.guild-webhooks-tab.strong">{channelWebhooksSettingsPath}</strong>. You
						can still edit and organize all existing webhooks here.
					</Trans>
				</div>
			)}
			{fetchStatus === 'pending' && (
				<div className={styles.spinnerContainer} data-flx="guild.guild-tabs.guild-webhooks-tab.spinner-container">
					<Spinner data-flx="guild.guild-tabs.guild-webhooks-tab.spinner" />
				</div>
			)}
			{fetchStatus === 'error' && (
				<StatusSlate
					Icon={WarningCircleIcon}
					title={i18n._(FAILED_TO_LOAD_WEBHOOKS_DESCRIPTOR)}
					description={i18n._(THERE_WAS_AN_ERROR_LOADING_THE_WEBHOOKS_PLEASE_DESCRIPTOR)}
					actions={[
						{
							text: i18n._(TRY_AGAIN_DESCRIPTOR),
							onClick: () => WebhookCommands.fetchGuildWebhooks?.(guildId),
							variant: 'primary',
						},
					]}
					fullHeight={true}
					data-flx="guild.guild-tabs.guild-webhooks-tab.status-slate"
				/>
			)}
			{fetchStatus === 'success' && incomingWebhooks.length > 0 && (
				<div className={styles.webhookList} data-flx="guild.guild-tabs.guild-webhooks-tab.webhook-list">
					{incomingWebhooks.map((webhook: Webhook) => (
						<WebhookListItem
							key={webhook.id}
							webhook={webhook}
							channelName={channelNameMap.get(webhook.channelId) ?? undefined}
							onUpdate={handleUpdate}
							onDelete={(webhook) => WebhookCommands.deleteWebhook(webhook.id)}
							availableChannels={availableChannels}
							defaultExpanded={false}
							isExpanded={expandedIds.has(webhook.id)}
							onExpandedChange={(open) => setExpanded(webhook.id, open)}
							formVersion={formVersion}
							data-flx="guild.guild-tabs.guild-webhooks-tab.webhook-list-item"
						/>
					))}
				</div>
			)}
			{fetchStatus === 'success' && incomingWebhooks.length === 0 && (
				<StatusSlate
					Icon={RobotIcon}
					title={<Trans>No webhooks</Trans>}
					description={
						<Trans>
							This community doesn't have any webhooks yet. Go to {channelWebhooksSettingsPath} to create one.
						</Trans>
					}
					fullHeight={followerWebhookGroups.length === 0}
					data-flx="guild.guild-tabs.guild-webhooks-tab.status-slate--2"
				/>
			)}
			{fetchStatus === 'success' && followerWebhookGroups.length > 0 && (
				<section className={styles.followedSection} data-flx="guild.guild-tabs.guild-webhooks-tab.followed-section">
					<div data-flx="guild.guild-tabs.guild-webhooks-tab.followed-header">
						<h3 className={styles.sectionTitle} data-flx="guild.guild-tabs.guild-webhooks-tab.followed-title">
							<Trans comment="Section heading in the community webhook settings that lists announcement channels followed into this community.">
								Followed channels
							</Trans>
						</h3>
						<p
							className={styles.sectionDescription}
							data-flx="guild.guild-tabs.guild-webhooks-tab.followed-description"
						>
							<Trans comment="Description of the followed channels section in the community webhook settings.">
								Messages published in these announcement channels are copied into your channels.
							</Trans>
						</p>
					</div>
					{followerWebhookGroups.map((group) => {
						const targetChannelName = group.channelName;
						return (
							<div
								key={group.channelId}
								className={styles.followedGroup}
								data-flx="guild.guild-tabs.guild-webhooks-tab.followed-group"
							>
								<h4
									className={styles.followedGroupTitle}
									data-flx="guild.guild-tabs.guild-webhooks-tab.followed-group-title"
								>
									<Trans comment="Group heading for followed announcement channels that post into one channel. targetChannelName is that channel, without the leading #.">
										Delivered to #{targetChannelName}
									</Trans>
								</h4>
								<div className={styles.webhookList} data-flx="guild.guild-tabs.guild-webhooks-tab.followed-list">
									{group.items.map((webhook) => (
										<FollowedChannelListItem
											key={webhook.id}
											webhook={webhook}
											onUpdate={handleUpdate}
											isExpanded={expandedIds.has(webhook.id)}
											onExpandedChange={(open) => setExpanded(webhook.id, open)}
											formVersion={formVersion}
											data-flx="guild.guild-tabs.guild-webhooks-tab.followed-channel-list-item"
										/>
									))}
								</div>
							</div>
						);
					})}
				</section>
			)}
		</div>
	);
});

export default GuildWebhooksTab;
