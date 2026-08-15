// SPDX-License-Identifier: AGPL-3.0-or-later

import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {CopyLinkSection} from '@app/features/app/components/dialogs/shared/CopyLinkSection';
import type {RecipientItem} from '@app/features/app/components/dialogs/shared/RecipientList';
import {RecipientList, useRecipientItems} from '@app/features/app/components/dialogs/shared/RecipientList';
import selectorStyles from '@app/features/app/components/dialogs/shared/SelectorModalStyles.module.css';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import {
	CUSTOM_ELLIPSIS_DESCRIPTOR,
	FAILED_TO_SEND_INVITE_DESCRIPTOR,
	NEVER_DESCRIPTOR,
	ONE_DAY_DURATION_DESCRIPTOR,
	ONE_HOUR_DURATION_DESCRIPTOR,
	SEARCH_FRIENDS_DESCRIPTOR,
	SENT_DESCRIPTOR,
	SEVEN_DAYS_DURATION_DESCRIPTOR,
	SIX_HOURS_DURATION_DESCRIPTOR,
	SOMETHING_WENT_WRONG_DESCRIPTOR,
	THIRTY_MINUTES_DURATION_DESCRIPTOR,
	TWELVE_HOURS_DURATION_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import styles from '@app/features/invite/components/modals/InviteModal.module.css';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {Logger} from '@app/features/platform/utils/AppLogger';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Combobox} from '@app/features/ui/components/form/FormCombobox';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {Spinner} from '@app/features/ui/components/Spinner';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {useCopyLinkHandler} from '@app/lib/copy-link';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {MAX_INVITE_AGE_SECONDS, MAX_INVITE_USES} from '@fluxer/constants/src/LimitConstants';
import type {Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon, WarningCircleIcon, WarningIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useState} from 'react';

const NO_LIMIT_DESCRIPTOR = msg({
	message: 'No limit',
	comment: 'Invite max-uses option meaning unlimited uses. Short standalone label.',
});
const MESSAGE_1_USE_DESCRIPTOR = msg({
	message: '1 use',
	comment: 'Invite max-uses dropdown option for exactly one use.',
});
const MESSAGE_5_USES_DESCRIPTOR = msg({
	message: '5 uses',
	comment: 'Invite max-uses dropdown option for five uses.',
});
const MESSAGE_10_USES_DESCRIPTOR = msg({
	message: '10 uses',
	comment: 'Invite max-uses dropdown option for ten uses.',
});
const MESSAGE_25_USES_DESCRIPTOR = msg({
	message: '25 uses',
	comment: 'Invite max-uses dropdown option for 25 uses.',
});
const MESSAGE_50_USES_DESCRIPTOR = msg({
	message: '50 uses',
	comment: 'Invite max-uses dropdown option for 50 uses.',
});
const MESSAGE_100_USES_DESCRIPTOR = msg({
	message: '100 uses',
	comment: 'Invite max-uses dropdown option for 100 uses.',
});
const INVITE_FRIENDS_DESCRIPTOR = msg({
	message: 'Invite friends',
	comment:
		'Fallback header on the invite modal when no community context is available. Refers to inviting friends to Fluxer.',
});
const INVITE_FRIENDS_TO_DESCRIPTOR = msg({
	message: 'Invite friends to {guildName}',
	comment: 'Title of the invite modal. {guildName} is the community name.',
});
const INVITE_LINK_SETTINGS_DESCRIPTOR = msg({
	message: 'Invite link settings',
	comment: 'Header of the invite modal when the user is editing the invite link expiry, max uses, and temporary flag.',
});
const INVITE_DESCRIPTOR = msg({
	message: 'Invite',
	comment:
		'Per-recipient action button label in the invite modal recipient list. Sends the invite as a DM to that friend. Short standalone verb.',
});
const EXPIRE_AFTER_DESCRIPTOR = msg({
	message: 'Expire after',
	comment: 'Label above the expiration duration dropdown in the invite link settings.',
});
const CUSTOM_EXPIRY_SECONDS_DESCRIPTOR = msg({
	message: 'Custom expiry (seconds)',
	comment:
		'Label of the numeric input shown after choosing "Custom..." in the invite expiration dropdown. The unit is seconds.',
});
const ANY_VALUE_FROM_0_TO_SECONDS_0_MEANS_DESCRIPTOR = msg({
	message: 'Any value from 0 to {maxInviteAgeSeconds} seconds. 0 means never expires.',
	comment:
		'Helper text under the custom invite expiration input. {maxInviteAgeSeconds} is the platform max in seconds.',
});
const MAX_NUMBER_OF_USES_DESCRIPTOR = msg({
	message: 'Max number of uses',
	comment: 'Label above the max-uses dropdown in the invite link settings.',
});
const CUSTOM_MAX_USES_DESCRIPTOR = msg({
	message: 'Custom max uses',
	comment: 'Label of the numeric input shown after choosing "Custom..." in the invite max-uses dropdown.',
});
const ANY_VALUE_FROM_0_TO_0_MEANS_NO_DESCRIPTOR = msg({
	message: 'Any value from 0 to {maxInviteUses}. 0 means no limit.',
	comment:
		'Helper text under the custom invite max-uses input. {maxInviteUses} is the platform max number of uses per invite.',
});
const GRANT_TEMPORARY_MEMBERSHIP_DESCRIPTOR = msg({
	message: 'Temporary membership',
	comment:
		'Switch label in the invite link settings. When enabled, members joining via this invite are kicked when they go offline unless given a role.',
});
const MEMBERS_WILL_BE_REMOVED_WHEN_THEY_GO_OFFLINE_DESCRIPTOR = msg({
	message: 'Members are removed when offline unless given a role.',
	comment: 'Helper text under the temporary membership switch in the invite link settings.',
});
const INVITE_LINK_DESCRIPTOR = msg({
	message: 'Invite link',
	comment: 'Placeholder in the read-only invite link input when no link is available yet.',
});
const LINK_HIDDEN_WHILE_SHARING_DESCRIPTOR = msg({
	message: 'Link hidden while sharing',
	comment: 'Replacement text for an invite URL while streaming privacy is active.',
});
const INVITE_LINK_HIDDEN_LABEL_DESCRIPTOR = msg({
	message: 'Invite link is hidden while sharing:',
	comment: 'Label above a masked invite link while streaming privacy is active.',
});
const logger = new Logger('InviteModal');

type InviteModalView = 'recipients' | 'advanced';

const INVITE_MODAL_VIEW_ORDER: ReadonlyArray<InviteModalView> = ['recipients', 'advanced'];

interface InviteModalContentProps {
	channelId: string;
	channel: Channel;
	guild: Guild;
	inviteCapability: InviteUtils.InviteCapability;
}

const InviteModalUnavailable = observer(function InviteModalUnavailable() {
	const {i18n} = useLingui();
	return (
		<Modal.Root size="small" centered data-flx="invite.invite-modal.modal-root">
			<Modal.Header title={i18n._(INVITE_FRIENDS_DESCRIPTOR)} data-flx="invite.invite-modal.modal-header" />
			<Modal.Content className={styles.noChannelContent} data-flx="invite.invite-modal.no-channel-content">
				<WarningIcon
					size={remFromPx(48)}
					weight="fill"
					className={styles.noChannelIcon}
					data-flx="invite.invite-modal.no-channel-icon"
				/>
				<p className={styles.noChannelText} data-flx="invite.invite-modal.no-channel-text">
					<Trans>There are no channels available to create an invite for.</Trans>
				</p>
			</Modal.Content>
		</Modal.Root>
	);
});
const InviteModalContent = observer(function InviteModalContent({
	channelId,
	channel,
	guild,
	inviteCapability,
}: InviteModalContentProps) {
	const {i18n} = useLingui();
	const isUsingVanityUrl = inviteCapability.useVanityUrl;
	const [invite, setInvite] = useState<Invite | null>(null);
	const [loading, setLoading] = useState(!isUsingVanityUrl);
	const [showAdvanced, setShowAdvanced] = useState(false);
	const [sentInvites, setSentInvites] = useState(new Map<string, boolean>());
	const [sendingTo, setSendingTo] = useState(new Set<string>());
	const [maxAge, setMaxAge] = useState('604800');
	const [maxUses, setMaxUses] = useState('0');
	const [temporary, setTemporary] = useState(false);
	const [isMaxAgeCustom, setIsMaxAgeCustom] = useState(false);
	const [isMaxUsesCustom, setIsMaxUsesCustom] = useState(false);
	const recipients = useRecipientItems();
	const [searchQuery, setSearchQuery] = useState('');
	const CUSTOM_OPTION = '__custom__';
	const maxAgeOptions = useMemo(
		() => [
			{value: '0', label: i18n._(NEVER_DESCRIPTOR)},
			{value: '1800', label: i18n._(THIRTY_MINUTES_DURATION_DESCRIPTOR)},
			{value: '3600', label: i18n._(ONE_HOUR_DURATION_DESCRIPTOR)},
			{value: '21600', label: i18n._(SIX_HOURS_DURATION_DESCRIPTOR)},
			{value: '43200', label: i18n._(TWELVE_HOURS_DURATION_DESCRIPTOR)},
			{value: '86400', label: i18n._(ONE_DAY_DURATION_DESCRIPTOR)},
			{value: '604800', label: i18n._(SEVEN_DAYS_DURATION_DESCRIPTOR)},
			{value: CUSTOM_OPTION, label: i18n._(CUSTOM_ELLIPSIS_DESCRIPTOR)},
		],
		[i18n.locale],
	);
	const maxUsesOptions = useMemo(
		() => [
			{value: '0', label: i18n._(NO_LIMIT_DESCRIPTOR)},
			{value: '1', label: i18n._(MESSAGE_1_USE_DESCRIPTOR)},
			{value: '5', label: i18n._(MESSAGE_5_USES_DESCRIPTOR)},
			{value: '10', label: i18n._(MESSAGE_10_USES_DESCRIPTOR)},
			{value: '25', label: i18n._(MESSAGE_25_USES_DESCRIPTOR)},
			{value: '50', label: i18n._(MESSAGE_50_USES_DESCRIPTOR)},
			{value: '100', label: i18n._(MESSAGE_100_USES_DESCRIPTOR)},
			{value: CUSTOM_OPTION, label: i18n._(CUSTOM_ELLIPSIS_DESCRIPTOR)},
		],
		[i18n.locale],
	);
	const clampNumber = (raw: string, max: number): string => {
		if (raw === '') return '0';
		const parsed = Number.parseInt(raw, 10);
		if (Number.isNaN(parsed)) return '0';
		return String(Math.max(0, Math.min(max, parsed)));
	};
	const loadInvite = useCallback(
		async (options?: {maxAge?: number; maxUses?: number; temporary?: boolean}) => {
			if (isUsingVanityUrl) {
				return;
			}
			setLoading(true);
			try {
				const newInvite = await InviteCommands.create(channelId, {
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
	const title = i18n._(INVITE_FRIENDS_TO_DESCRIPTOR, {guildName: guild.name});
	const invitesDisabled = guild.features.has(GuildFeatures.INVITES_DISABLED);
	const inviteUrl = isUsingVanityUrl
		? InviteUtils.getVanityInviteUrl(inviteCapability.vanityUrlCode!)
		: invite
			? `${RuntimeConfig.inviteEndpoint}/${invite.code}`
			: '';
	const hideInviteLinks = StreamerMode.shouldHideInviteLinks;
	const displayedInviteUrl = hideInviteLinks && inviteUrl ? i18n._(LINK_HIDDEN_WHILE_SHARING_DESCRIPTOR) : inviteUrl;
	const handleCopy = useCopyLinkHandler(inviteUrl, true);
	const handleSendInvite = async (item: RecipientItem) => {
		const userId = item.type === 'group_dm' ? item.id : item.user.id;
		setSendingTo((prev) => new Set(prev).add(userId));
		let targetChannelId: string;
		if (item.channelId) {
			targetChannelId = item.channelId;
		} else {
			targetChannelId = await PrivateChannelCommands.ensureDMChannel(item.user.id);
		}
		try {
			const result = await MessageCommands.send(targetChannelId, {
				content: inviteUrl,
				nonce: SnowflakeUtils.fromTimestamp(Date.now()),
			});
			if (result) {
				setSentInvites((prev) => new Map(prev).set(userId, true));
			}
		} catch (error) {
			logger.error('Failed to send invite:', error);
			showGenericErrorModal({
				title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
				message: () => i18n._(FAILED_TO_SEND_INVITE_DESCRIPTOR),
				dataFlx: 'invite.invite-modal.send-invite-error-modal',
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
		const option = maxAgeOptions.find((opt) => opt.value === maxAge);
		if (option) {
			return option.label;
		}
		return maxAge;
	};
	return (
		<Modal.Root size="small" centered data-flx="invite.invite-modal.modal-root--2">
			<Modal.Header
				title={!showAdvanced ? title : i18n._(INVITE_LINK_SETTINGS_DESCRIPTOR)}
				data-flx="invite.invite-modal.modal-header--2"
			>
				{!showAdvanced && (
					<>
						<p
							className={clsx(selectorStyles.subtitle, styles.channelSubtitle)}
							data-flx="invite.invite-modal.channel-subtitle"
						>
							<Trans>
								Recipients will be taken to {ChannelUtils.getIcon(channel, {size: 16, className: styles.channelIcon})}{' '}
								<span className={styles.channelName} data-flx="invite.invite-modal.channel-name">
									{channel.name}
								</span>
							</Trans>
						</p>
						{invitesDisabled && (
							<div className={styles.warningContainer} data-flx="invite.invite-modal.warning-container">
								<WarningCircleIcon
									className={styles.warningIcon}
									weight="fill"
									data-flx="invite.invite-modal.warning-icon"
								/>
								<p className={styles.warningText} data-flx="invite.invite-modal.warning-text">
									<Trans>
										Invites are paused in this community by an admin. While this invite can be created, it cannot be
										accepted until invites are re-enabled.
									</Trans>
								</p>
							</div>
						)}
						<div className={selectorStyles.headerSearch} data-flx="invite.invite-modal.div">
							<Input
								value={searchQuery}
								onChange={(e) => setSearchQuery(e.target.value)}
								placeholder={i18n._(SEARCH_FRIENDS_DESCRIPTOR)}
								leftIcon={
									<MagnifyingGlassIcon
										size={remFromPx(20)}
										weight="bold"
										className={selectorStyles.searchIcon}
										data-flx="invite.invite-modal.magnifying-glass-icon"
									/>
								}
								className={selectorStyles.headerSearchInput}
								data-flx="invite.invite-modal.input.set-search-query"
							/>
						</div>
					</>
				)}
			</Modal.Header>
			<Modal.Content className={selectorStyles.selectorContent} data-flx="invite.invite-modal.modal-content">
				{loading ? (
					<div className={styles.loadingContainer} data-flx="invite.invite-modal.loading-container">
						<Spinner data-flx="invite.invite-modal.spinner" />
					</div>
				) : (
					<SteppedCarousel
						step={showAdvanced ? 'advanced' : 'recipients'}
						steps={INVITE_MODAL_VIEW_ORDER}
						data-flx="invite.invite-modal.stepped-carousel"
					>
						{!showAdvanced ? (
							<RecipientList
								recipients={recipients}
								sendingTo={sendingTo}
								sentTo={sentInvites}
								onSend={handleSendInvite}
								defaultButtonLabel={i18n._(INVITE_DESCRIPTOR)}
								sentButtonLabel={i18n._(SENT_DESCRIPTOR)}
								buttonClassName={styles.inviteButton}
								scrollerKey="invite-modal-friend-list-scroller"
								searchQuery={searchQuery}
								onSearchQueryChange={setSearchQuery}
								showSearchInput={false}
								data-flx="invite.invite-modal.recipient-list"
							/>
						) : (
							<div className={styles.detailedView} data-flx="invite.invite-modal.advanced-view">
								<Combobox
									label={i18n._(EXPIRE_AFTER_DESCRIPTOR)}
									options={maxAgeOptions}
									value={isMaxAgeCustom ? CUSTOM_OPTION : maxAge}
									onChange={(value) => {
										if (value == null) return;
										if (value === CUSTOM_OPTION) {
											setIsMaxAgeCustom(true);
											return;
										}
										setIsMaxAgeCustom(false);
										setMaxAge(String(value));
									}}
									data-flx="invite.invite-modal.select.set-is-max-age-custom"
								/>
								{isMaxAgeCustom && (
									<Input
										type="number"
										label={i18n._(CUSTOM_EXPIRY_SECONDS_DESCRIPTOR)}
										footer={i18n._(ANY_VALUE_FROM_0_TO_SECONDS_0_MEANS_DESCRIPTOR, {
											maxInviteAgeSeconds: MAX_INVITE_AGE_SECONDS,
										})}
										min={0}
										max={MAX_INVITE_AGE_SECONDS}
										step={1}
										value={maxAge}
										onChange={(event) => setMaxAge(clampNumber(event.target.value, MAX_INVITE_AGE_SECONDS))}
										data-flx="invite.invite-modal.input.set-max-age.number"
									/>
								)}
								<Combobox
									label={i18n._(MAX_NUMBER_OF_USES_DESCRIPTOR)}
									options={maxUsesOptions}
									value={isMaxUsesCustom ? CUSTOM_OPTION : maxUses}
									onChange={(value) => {
										if (value == null) return;
										if (value === CUSTOM_OPTION) {
											setIsMaxUsesCustom(true);
											return;
										}
										setIsMaxUsesCustom(false);
										setMaxUses(String(value));
									}}
									data-flx="invite.invite-modal.select.set-is-max-uses-custom"
								/>
								{isMaxUsesCustom && (
									<Input
										type="number"
										label={i18n._(CUSTOM_MAX_USES_DESCRIPTOR)}
										footer={i18n._(ANY_VALUE_FROM_0_TO_0_MEANS_NO_DESCRIPTOR, {maxInviteUses: MAX_INVITE_USES})}
										min={0}
										max={MAX_INVITE_USES}
										step={1}
										value={maxUses}
										onChange={(event) => setMaxUses(clampNumber(event.target.value, MAX_INVITE_USES))}
										data-flx="invite.invite-modal.input.set-max-uses.number"
									/>
								)}
								<Switch
									label={i18n._(GRANT_TEMPORARY_MEMBERSHIP_DESCRIPTOR)}
									description={i18n._(MEMBERS_WILL_BE_REMOVED_WHEN_THEY_GO_OFFLINE_DESCRIPTOR)}
									value={temporary}
									onChange={setTemporary}
									data-flx="invite.invite-modal.switch.set-temporary"
								/>
							</div>
						)}
					</SteppedCarousel>
				)}
			</Modal.Content>
			{!showAdvanced ? (
				<Modal.Footer data-flx="invite.invite-modal.modal-footer--2">
					<CopyLinkSection
						label={
							hideInviteLinks ? (
								i18n._(INVITE_LINK_HIDDEN_LABEL_DESCRIPTOR)
							) : (
								<Trans>Or send an invite link to a friend:</Trans>
							)
						}
						value={displayedInviteUrl}
						onCopy={handleCopy}
						copyDisabled={hideInviteLinks}
						onInputClick={hideInviteLinks ? undefined : (e) => e.currentTarget.select()}
						inputProps={{placeholder: i18n._(INVITE_LINK_DESCRIPTOR)}}
						data-flx="invite.invite-modal.copy-link-section"
					>
						{isUsingVanityUrl || maxAge === '0' ? (
							<p className={styles.expirationText} data-flx="invite.invite-modal.expiration-text">
								<Trans>This invite link never expires.</Trans>{' '}
								{!isUsingVanityUrl && (
									<FocusRing offset={-2} data-flx="invite.invite-modal.focus-ring">
										<button
											type="button"
											onClick={() => setShowAdvanced(true)}
											className={styles.editLink}
											data-flx="invite.invite-modal.edit-link.set-show-advanced.button"
										>
											<Trans>Edit invite link</Trans>
										</button>
									</FocusRing>
								)}
							</p>
						) : (
							<p className={styles.expirationText} data-flx="invite.invite-modal.expiration-text--2">
								<Trans>Your invite link expires in {getExpirationText()}.</Trans>{' '}
								<FocusRing offset={-2} data-flx="invite.invite-modal.focus-ring--2">
									<button
										type="button"
										onClick={() => setShowAdvanced(true)}
										className={styles.editLink}
										data-flx="invite.invite-modal.edit-link.set-show-advanced.button--2"
									>
										<Trans>Edit invite link</Trans>
									</button>
								</FocusRing>
							</p>
						)}
					</CopyLinkSection>
				</Modal.Footer>
			) : (
				<Modal.Footer data-flx="invite.invite-modal.modal-footer--3">
					<Button
						variant="secondary"
						onClick={() => setShowAdvanced(false)}
						data-flx="invite.invite-modal.button.set-show-advanced"
					>
						<Trans>Cancel</Trans>
					</Button>
					<Button onClick={handleGenerateNew} data-flx="invite.invite-modal.button.generate-new">
						<Trans>Create new link</Trans>
					</Button>
				</Modal.Footer>
			)}
		</Modal.Root>
	);
});
export const InviteModal = observer(({channelId}: {channelId: string}) => {
	const channel = Channels.getChannel(channelId);
	if (!channel || channel.guildId == null) {
		return <InviteModalUnavailable data-flx="invite.invite-modal.invite-modal-unavailable" />;
	}
	const guild = Guilds.getGuild(channel.guildId);
	if (!guild) {
		return <InviteModalUnavailable data-flx="invite.invite-modal.invite-modal-unavailable--2" />;
	}
	const inviteCapability = InviteUtils.getInviteCapability(channelId, channel.guildId);
	return (
		<InviteModalContent
			channelId={channelId}
			channel={channel}
			guild={guild}
			inviteCapability={inviteCapability}
			data-flx="invite.invite-modal.invite-modal-content"
		/>
	);
});
