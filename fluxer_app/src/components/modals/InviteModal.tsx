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
import {MagnifyingGlassIcon, WarningCircleIcon, WarningIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as InviteActionCreators from '~/actions/InviteActionCreators';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {GuildFeatures} from '~/Constants';
import {Input} from '~/components/form/Input';
import {Select} from '~/components/form/Select';
import {Switch} from '~/components/form/Switch';
import styles from '~/components/modals/InviteModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {CopyLinkSection} from '~/components/modals/shared/CopyLinkSection';
import type {RecipientItem} from '~/components/modals/shared/RecipientList';
import {RecipientList, useRecipientItems} from '~/components/modals/shared/RecipientList';
import selectorStyles from '~/components/modals/shared/SelectorModalStyles.module.css';
import {Button} from '~/components/uikit/Button/Button';
import {Spinner} from '~/components/uikit/Spinner';
import type {Invite} from '~/records/MessageRecord';
import ChannelStore from '~/stores/ChannelStore';
import GuildStore from '~/stores/GuildStore';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import * as ChannelUtils from '~/utils/ChannelUtils';
import * as InviteUtils from '~/utils/InviteUtils';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';

export const InviteModal = observer(({channelId}: {channelId: string}) => {
	const {t, i18n} = useLingui();
	const channel = ChannelStore.getChannel(channelId);
	const inviteCapability = InviteUtils.getInviteCapability(channelId, channel?.guildId);
	const isUsingVanityUrl = inviteCapability.useVanityUrl;

	const [invite, setInvite] = React.useState<Invite | null>(null);
	const [loading, setLoading] = React.useState(!isUsingVanityUrl);
	const [showAdvanced, setShowAdvanced] = React.useState(false);
	const [copied, setCopied] = React.useState(false);
	const [sentInvites, setSentInvites] = React.useState(new Map<string, boolean>());
	const [sendingTo, setSendingTo] = React.useState(new Set<string>());

	const [maxAge, setMaxAge] = React.useState('604800');
	const [maxUses, setMaxUses] = React.useState('0');
	const [temporary, setTemporary] = React.useState(false);
	const recipients = useRecipientItems();
	const [searchQuery, setSearchQuery] = React.useState('');

	const maxAgeOptions = React.useMemo(
		() => [
			{value: '0', label: t`Never`},
			{value: '1800', label: t`30 minutes`},
			{value: '3600', label: t`1 hour`},
			{value: '21600', label: t`6 hours`},
			{value: '43200', label: t`12 hours`},
			{value: '86400', label: t`1 day`},
			{value: '604800', label: t`7 days`},
		],
		[t],
	);

	const maxUsesOptions = React.useMemo(
		() => [
			{value: '0', label: t`No limit`},
			{value: '1', label: t`1 use`},
			{value: '5', label: t`5 uses`},
			{value: '10', label: t`10 uses`},
			{value: '25', label: t`25 uses`},
			{value: '50', label: t`50 uses`},
			{value: '100', label: t`100 uses`},
		],
		[t],
	);

	const loadInvite = React.useCallback(
		async (options?: {maxAge?: number; maxUses?: number; temporary?: boolean}) => {
			if (isUsingVanityUrl) {
				return;
			}
			setLoading(true);
			try {
				const newInvite = await InviteActionCreators.create(channelId, {
					max_age: options?.maxAge,
					max_uses: options?.maxUses,
					temporary: options?.temporary,
				});
				setInvite(newInvite);
			} finally {
				setLoading(false);
			}
		},
		[channelId, isUsingVanityUrl],
	);

	React.useEffect(() => {
		if (!isUsingVanityUrl) {
			loadInvite({maxAge: 604800, maxUses: 0, temporary: false});
		}
	}, [loadInvite, isUsingVanityUrl]);
	if (!channel || channel.guildId == null) {
		return (
			<Modal.Root size="small" centered>
				<Modal.Header title={t`Invite friends`} />
				<Modal.Content className={styles.noChannelContent}>
					<WarningIcon size={48} weight="fill" className={styles.noChannelIcon} />
					<p className={styles.noChannelText}>
						<Trans>There are no channels available to create an invite for.</Trans>
					</p>
				</Modal.Content>
				<Modal.Footer>
					<Button onClick={() => ModalActionCreators.pop()}>{t`Close`}</Button>
				</Modal.Footer>
			</Modal.Root>
		);
	}
	const guild = GuildStore.getGuild(channel.guildId)!;
	const title = t`Invite friends to ${guild.name}`;
	const invitesDisabled = guild.features.has(GuildFeatures.INVITES_DISABLED);

	const inviteUrl = isUsingVanityUrl
		? InviteUtils.getVanityInviteUrl(inviteCapability.vanityUrlCode!)
		: invite
			? `${RuntimeConfigStore.inviteEndpoint}/${invite.code}`
			: '';

	const handleCopy = async () => {
		if (!inviteUrl) return;
		await TextCopyActionCreators.copy(i18n, inviteUrl, true);
		setCopied(true);
		setTimeout(() => setCopied(false), 2000);
	};

	const handleSendInvite = async (item: RecipientItem) => {
		const userId = item.type === 'group_dm' ? item.id : item.user.id;

		setSendingTo((prev) => new Set(prev).add(userId));
		try {
			let targetChannelId: string;
			if (item.channelId) {
				targetChannelId = item.channelId;
			} else {
				targetChannelId = await PrivateChannelActionCreators.ensureDMChannel(item.user.id);
			}

			await MessageActionCreators.send(targetChannelId, {
				content: inviteUrl,
				nonce: SnowflakeUtils.fromTimestamp(Date.now()),
			});

			setSentInvites((prev) => new Map(prev).set(userId, true));
		} catch (error) {
			console.error('Failed to send invite:', error);
			ToastActionCreators.createToast({
				type: 'error',
				children: <Trans>Failed to send invite</Trans>,
			});
		} finally {
			setSendingTo((prev) => {
				const next = new Set(prev);
				next.delete(userId);
				return next;
			});
		}
	};

	const handleGenerateNew = () => {
		loadInvite({
			maxAge: parseInt(maxAge, 10),
			maxUses: parseInt(maxUses, 10),
			temporary,
		});
		setShowAdvanced(false);
	};

	const getExpirationText = () => {
		if (maxAge === '0') {
			return <Trans>never expires</Trans>;
		}
		const option = maxAgeOptions.find((opt) => opt.value === maxAge);
		if (option) {
			switch (option.value) {
				case '1800':
					return <Trans>30 minutes</Trans>;
				case '3600':
					return <Trans>1 hour</Trans>;
				case '21600':
					return <Trans>6 hours</Trans>;
				case '43200':
					return <Trans>12 hours</Trans>;
				case '86400':
					return <Trans>1 day</Trans>;
				case '604800':
					return <Trans>7 days</Trans>;
				default:
					return option.label;
			}
		}
		return maxAge;
	};

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={!showAdvanced ? title : t`Invite link settings`}>
				{!showAdvanced && (
					<>
						<p className={clsx(selectorStyles.subtitle, styles.channelSubtitle)}>
							<Trans>
								Recipients will be taken to {ChannelUtils.getIcon(channel, {size: 16, className: styles.channelIcon})}{' '}
								<span className={styles.channelName}>{channel.name}</span>
							</Trans>
						</p>
						<div className={selectorStyles.headerSearch}>
							<Input
								value={searchQuery}
								onChange={(e) => setSearchQuery(e.target.value)}
								placeholder={t`Search friends`}
								leftIcon={<MagnifyingGlassIcon size={20} weight="bold" className={selectorStyles.searchIcon} />}
								className={selectorStyles.headerSearchInput}
							/>
						</div>
					</>
				)}
			</Modal.Header>
			<Modal.Content className={selectorStyles.selectorContent}>
				{loading ? (
					<div className={styles.loadingContainer}>
						<Spinner />
					</div>
				) : !showAdvanced ? (
					<>
						<RecipientList
							recipients={recipients}
							sendingTo={sendingTo}
							sentTo={sentInvites}
							onSend={handleSendInvite}
							defaultButtonLabel={t`Invite`}
							sentButtonLabel={t`Sent`}
							buttonClassName={styles.inviteButton}
							scrollerKey="invite-modal-friend-list-scroller"
							searchQuery={searchQuery}
							onSearchQueryChange={setSearchQuery}
							showSearchInput={false}
						/>

						{invitesDisabled && (
							<div className={styles.warningContainer}>
								<WarningCircleIcon className={styles.warningIcon} weight="fill" />
								<p className={styles.warningText}>
									<Trans>
										Invites are currently disabled in this community by an admin. While this invite can be created, it
										cannot be accepted until invites are re-enabled.
									</Trans>
								</p>
							</div>
						)}
					</>
				) : (
					<div className={styles.advancedView}>
						<Select
							label={t`Expire after`}
							options={maxAgeOptions}
							value={maxAge}
							onChange={(value) => {
								if (value == null) return;
								setMaxAge(String(value));
							}}
						/>

						<Select
							label={t`Max number of uses`}
							options={maxUsesOptions}
							value={maxUses}
							onChange={(value) => {
								if (value == null) return;
								setMaxUses(String(value));
							}}
						/>

						<Switch
							label={t`Grant temporary membership`}
							description={t`Members will be removed when they go offline unless a role is assigned`}
							value={temporary}
							onChange={setTemporary}
						/>
					</div>
				)}
			</Modal.Content>
			{!showAdvanced ? (
				<Modal.Footer>
					<CopyLinkSection
						label={<Trans>or send an invite link to a friend:</Trans>}
						value={inviteUrl}
						onCopy={handleCopy}
						copied={copied}
						onInputClick={(e) => e.currentTarget.select()}
						inputProps={{placeholder: t`Invite link`}}
					>
						{isUsingVanityUrl ? (
							<p className={styles.expirationText}>
								<Trans>This invite link never expires.</Trans>
							</p>
						) : (
							<p className={styles.expirationText}>
								<Trans>Your invite link expires in {getExpirationText()}.</Trans>{' '}
								<button type="button" onClick={() => setShowAdvanced(true)} className={styles.editLink}>
									<Trans>Edit invite link</Trans>
								</button>
							</p>
						)}
					</CopyLinkSection>
				</Modal.Footer>
			) : (
				<Modal.Footer>
					<Button variant="secondary" onClick={() => setShowAdvanced(false)}>
						<Trans>Cancel</Trans>
					</Button>
					<Button onClick={handleGenerateNew}>
						<Trans>Create New Link</Trans>
					</Button>
				</Modal.Footer>
			)}
		</Modal.Root>
	);
});
