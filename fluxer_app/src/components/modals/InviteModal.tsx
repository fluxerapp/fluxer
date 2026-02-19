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

import * as InviteActionCreators from '@app/actions/InviteActionCreators';
import * as MessageActionCreators from '@app/actions/MessageActionCreators';
import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import * as PrivateChannelActionCreators from '@app/actions/PrivateChannelActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import {Input} from '@app/components/form/Input';
import {Select} from '@app/components/form/Select';
import {Switch} from '@app/components/form/Switch';
import styles from '@app/components/modals/InviteModal.module.css';
import * as Modal from '@app/components/modals/Modal';
import {CopyLinkSection} from '@app/components/modals/shared/CopyLinkSection';
import type {RecipientItem} from '@app/components/modals/shared/RecipientList';
import {RecipientList, useRecipientItems} from '@app/components/modals/shared/RecipientList';
import selectorStyles from '@app/components/modals/shared/SelectorModalStyles.module.css';
import {Button} from '@app/components/uikit/button/Button';
import FocusRing from '@app/components/uikit/focus_ring/FocusRing';
import {Spinner} from '@app/components/uikit/Spinner';
import {Logger} from '@app/lib/Logger';
import ChannelStore from '@app/stores/ChannelStore';
import GuildStore from '@app/stores/GuildStore';
import RuntimeConfigStore from '@app/stores/RuntimeConfigStore';
import * as ChannelUtils from '@app/utils/ChannelUtils';
import {useCopyLinkHandler} from '@app/utils/CopyLinkHandlers';
import * as InviteUtils from '@app/utils/InviteUtils';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon, WarningCircleIcon, WarningIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useState} from 'react';

const logger = new Logger('InviteModal');

export const InviteModal = observer(({channelId}: {channelId: string}) => {
	const {t} = useLingui();
	const channel = ChannelStore.getChannel(channelId);
	const inviteCapability = InviteUtils.getInviteCapability(channelId, channel?.guildId);
	const isUsingVanityUrl = inviteCapability.useVanityUrl;

	const [invite, setInvite] = useState<Invite | null>(null);
	const [loading, setLoading] = useState(!isUsingVanityUrl);
	const [showAdvanced, setShowAdvanced] = useState(false);
	const [sentInvites, setSentInvites] = useState(new Map<string, boolean>());
	const [sendingTo, setSendingTo] = useState(new Set<string>());

	const [maxAge, setMaxAge] = useState('604800');
	const [maxUses, setMaxUses] = useState('0');
	const [temporary, setTemporary] = useState(false);
	const recipients = useRecipientItems();
	const [searchQuery, setSearchQuery] = useState('');

	const maxAgeOptions = useMemo(
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

	const maxUsesOptions = useMemo(
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

	const loadInvite = useCallback(
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

	useEffect(() => {
		if (!isUsingVanityUrl) {
			loadInvite({maxAge: 604800, maxUses: 0, temporary: false});
		}
	}, [loadInvite, isUsingVanityUrl]);
	if (!channel || channel.guildId == null) {
		return (
			<Modal.Root size="small" centered>
				<Modal.Header title={t`Invite Friends`} />
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

	const handleCopy = useCopyLinkHandler(inviteUrl, true);

	const handleSendInvite = async (item: RecipientItem) => {
		const userId = item.type === 'group_dm' ? item.id : item.user.id;

		setSendingTo((prev) => new Set(prev).add(userId));

		let targetChannelId: string;
		if (item.channelId) {
			targetChannelId = item.channelId;
		} else {
			targetChannelId = await PrivateChannelActionCreators.ensureDMChannel(item.user.id);
		}

		try {
			const result = await MessageActionCreators.send(targetChannelId, {
				content: inviteUrl,
				nonce: SnowflakeUtils.fromTimestamp(Date.now()),
			});

			if (result) {
				setSentInvites((prev) => new Map(prev).set(userId, true));
			}
		} catch (error) {
			logger.error('Failed to send invite:', error);
			ToastActionCreators.error(t`Failed to send invite. Please try again.`);
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
				) : (
					<div className={styles.advancedView}>
						<Select
							label={t`Expire After`}
							options={maxAgeOptions}
							value={maxAge}
							onChange={(value) => {
								if (value == null) return;
								setMaxAge(String(value));
							}}
						/>

						<Select
							label={t`Max Number of Uses`}
							options={maxUsesOptions}
							value={maxUses}
							onChange={(value) => {
								if (value == null) return;
								setMaxUses(String(value));
							}}
						/>

						<Switch
							label={t`Grant Temporary Membership`}
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
						onInputClick={(e) => e.currentTarget.select()}
						inputProps={{placeholder: t`Invite link`}}
					>
						{isUsingVanityUrl || maxAge === '0' ? (
							<p className={styles.expirationText}>
								<Trans>This invite link never expires.</Trans>{' '}
								{!isUsingVanityUrl && (
									<FocusRing offset={-2}>
										<button type="button" onClick={() => setShowAdvanced(true)} className={styles.editLink}>
											<Trans>Edit invite link</Trans>
										</button>
									</FocusRing>
								)}
							</p>
						) : (
							<p className={styles.expirationText}>
								<Trans>Your invite link expires in {getExpirationText()}.</Trans>{' '}
								<FocusRing offset={-2}>
									<button type="button" onClick={() => setShowAdvanced(true)} className={styles.editLink}>
										<Trans>Edit invite link</Trans>
									</button>
								</FocusRing>
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
