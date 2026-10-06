// SPDX-License-Identifier: AGPL-3.0-or-later

import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import {showChannelErrorModal} from '@app/features/channel/components/alerts/ChannelErrorModalUtils';
import styles from '@app/features/channel/components/modals/channel_tabs/ChannelWebhooksTab.module.css';
import Channels from '@app/features/channel/state/Channels';
import {UNKNOWN_CHANNEL_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {
	TRY_AGAIN_DESCRIPTOR,
	TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Permission from '@app/features/permissions/state/Permission';
import {formatPermissionLabel} from '@app/features/permissions/utils/PermissionUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Spinner} from '@app/features/ui/components/Spinner';
import {handleAccountLimitedError} from '@app/features/user/utils/AccountLimitUtils';
import * as WebhookCommands from '@app/features/webhook/commands/WebhookCommands';
import {FollowedChannelListItem} from '@app/features/webhook/components/FollowedChannelListItem';
import {WebhookListItem} from '@app/features/webhook/components/WebhookListItem';
import {useWebhookUpdates} from '@app/features/webhook/hooks/useWebhookUpdates';
import type {Webhook} from '@app/features/webhook/models/Webhook';
import Webhooks from '@app/features/webhook/state/Webhooks';
import {generateWebhookName} from '@app/features/webhook/utils/WebhookUtils';
import {GUILD_TEXT_BASED_CHANNEL_TYPES, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {RobotIcon, WarningOctagonIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

const WEBHOOK_CREATED_DESCRIPTOR = msg({
	message: 'Webhook created',
	comment: 'Short label in the channel webhooks tab. Keep it concise.',
});
const FAILED_TO_CREATE_WEBHOOK_DESCRIPTOR = msg({
	message: 'Failed to create webhook',
	comment: 'Error message in the channel webhooks tab.',
});
const CHANNEL_WEBHOOKS_TAB_ID = 'webhooks';
const logger = new Logger('ChannelWebhooksTab');
const ChannelWebhooksTab: React.FC<{channelId: string}> = observer(({channelId}) => {
	const {i18n} = useLingui();
	const channel = Channels.getChannel(channelId);
	const guildId = channel?.guildId ?? null;
	const canManageWebhooks =
		guildId && channel ? Permission.can(Permissions.MANAGE_WEBHOOKS, {channelId, guildId}) : false;
	const manageWebhooksPermissionLabel = formatPermissionLabel(i18n, Permissions.MANAGE_WEBHOOKS);
	const fetchStatus = Webhooks.getChannelFetchStatus(channelId);
	const webhooks = Webhooks.getChannelWebhooks(channelId);
	const guildChannels = Channels.getGuildChannels(guildId ?? '');
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
	const incomingWebhooks = useMemo(() => (webhooks ?? []).filter((webhook) => !webhook.isChannelFollower), [webhooks]);
	const followerWebhooks = useMemo(() => (webhooks ?? []).filter((webhook) => webhook.isChannelFollower), [webhooks]);
	const refreshWebhooks = useCallback(async () => {
		if (!guildId) return;
		try {
			await WebhookCommands.fetchChannelWebhooks({guildId, channelId});
		} catch (error) {
			logger.error('Failed to refresh webhooks', error);
		}
	}, [guildId, channelId]);
	useEffect(() => {
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
		<div data-flx="channel.channel-tabs.channel-webhooks-tab.div">
			<h2 className={styles.header} data-flx="channel.channel-tabs.channel-webhooks-tab.header">
				<Trans>Webhooks</Trans>
			</h2>
			<p className={styles.description} data-flx="channel.channel-tabs.channel-webhooks-tab.description">
				<Trans>Manage incoming webhooks that can post messages into this channel.</Trans>
			</p>
		</div>
	);
	const handleCreateQuick = useCallback(async () => {
		if (!canManageWebhooks) return;
		try {
			const name = generateWebhookName();
			await WebhookCommands.createWebhook({channelId, name});
			ToastCommands.createToast({type: 'success', children: i18n._(WEBHOOK_CREATED_DESCRIPTOR)});
			void WebhookCommands.fetchChannelWebhooks({guildId: guildId!, channelId}).catch(() => {});
		} catch (error) {
			logger.error('Failed to create webhook', error);
			if (handleAccountLimitedError(error)) return;
			showChannelErrorModal({
				title: i18n._(FAILED_TO_CREATE_WEBHOOK_DESCRIPTOR),
				message: i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
				dataFlx: 'channel.channel-tabs.channel-webhooks-tab.create-webhook-failed.generic-error-modal',
			});
		}
	}, [canManageWebhooks, channelId, guildId, i18n]);
	if (!channel || !guildId || !GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type)) {
		return (
			<div className={styles.container} data-flx="channel.channel-tabs.channel-webhooks-tab.container">
				{header}
				<div className={styles.messageBox} data-flx="channel.channel-tabs.channel-webhooks-tab.message-box">
					<Trans>This channel does not support webhooks.</Trans>
				</div>
			</div>
		);
	}
	return (
		<div className={styles.container} data-flx="channel.channel-tabs.channel-webhooks-tab.container--2">
			{header}
			{!canManageWebhooks && (
				<div className={styles.messageBox} data-flx="channel.channel-tabs.channel-webhooks-tab.message-box--2">
					<Trans>
						You need the "{manageWebhooksPermissionLabel}" permission to view and edit webhooks for this channel.
					</Trans>
				</div>
			)}
			{canManageWebhooks && (
				<div className={styles.buttonContainer} data-flx="channel.channel-tabs.channel-webhooks-tab.button-container">
					<Button
						onClick={handleCreateQuick}
						variant="primary"
						disabled={fetchStatus === 'pending'}
						small
						data-flx="channel.channel-tabs.channel-webhooks-tab.button.create-quick"
					>
						<Trans>Create webhook</Trans>
					</Button>
				</div>
			)}
			{fetchStatus === 'pending' && (
				<div className={styles.spinnerContainer} data-flx="channel.channel-tabs.channel-webhooks-tab.spinner-container">
					<Spinner data-flx="channel.channel-tabs.channel-webhooks-tab.spinner" />
				</div>
			)}
			{fetchStatus === 'error' && (
				<StatusSlate
					Icon={WarningOctagonIcon}
					title={<Trans>Failed to load webhooks</Trans>}
					description={<Trans>There was an error loading the webhooks for this channel. Try again.</Trans>}
					actions={[
						{
							text: i18n._(TRY_AGAIN_DESCRIPTOR),
							onClick: refreshWebhooks,
							variant: 'primary',
						},
					]}
					fullHeight={true}
					data-flx="channel.channel-tabs.channel-webhooks-tab.status-slate"
				/>
			)}
			{fetchStatus === 'success' && incomingWebhooks.length > 0 && (
				<div className={styles.webhooksList} data-flx="channel.channel-tabs.channel-webhooks-tab.webhooks-list">
					{incomingWebhooks.map((webhook: Webhook) => (
						<WebhookListItem
							key={webhook.id}
							webhook={webhook}
							onUpdate={handleUpdate}
							onDelete={(webhook) => WebhookCommands.deleteWebhook(webhook.id)}
							availableChannels={availableChannels}
							defaultExpanded={false}
							isExpanded={expandedIds.has(webhook.id)}
							onExpandedChange={(open) => setExpanded(webhook.id, open)}
							formVersion={formVersion}
							data-flx="channel.channel-tabs.channel-webhooks-tab.webhook-list-item"
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
							There are no webhooks configured for this channel. Create a webhook to allow external applications to post
							messages.
						</Trans>
					}
					actions={
						canManageWebhooks
							? [
									{
										text: <Trans>Create webhook</Trans>,
										onClick: handleCreateQuick,
										variant: 'primary',
									},
								]
							: undefined
					}
					fullHeight={followerWebhooks.length === 0}
					data-flx="channel.channel-tabs.channel-webhooks-tab.status-slate--2"
				/>
			)}
			{fetchStatus === 'success' && followerWebhooks.length > 0 && (
				<section
					className={styles.followedSection}
					data-flx="channel.channel-tabs.channel-webhooks-tab.followed-section"
				>
					<div data-flx="channel.channel-tabs.channel-webhooks-tab.followed-header">
						<h3 className={styles.sectionTitle} data-flx="channel.channel-tabs.channel-webhooks-tab.followed-title">
							<Trans comment="Section heading in the channel webhook settings that lists announcement channels this channel follows.">
								Followed channels
							</Trans>
						</h3>
						<p
							className={styles.sectionDescription}
							data-flx="channel.channel-tabs.channel-webhooks-tab.followed-description"
						>
							<Trans comment="Description of the followed channels section in the channel webhook settings.">
								Messages published in these announcement channels are copied here.
							</Trans>
						</p>
					</div>
					<div className={styles.webhooksList} data-flx="channel.channel-tabs.channel-webhooks-tab.followed-list">
						{followerWebhooks.map((webhook: Webhook) => (
							<FollowedChannelListItem
								key={webhook.id}
								webhook={webhook}
								onUpdate={handleUpdate}
								isExpanded={expandedIds.has(webhook.id)}
								onExpandedChange={(open) => setExpanded(webhook.id, open)}
								formVersion={formVersion}
								data-flx="channel.channel-tabs.channel-webhooks-tab.followed-channel-list-item"
							/>
						))}
					</div>
				</section>
			)}
		</div>
	);
});

export default ChannelWebhooksTab;
