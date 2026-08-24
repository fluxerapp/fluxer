// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {AuthErrorState} from '@app/features/auth/flow/AuthErrorState';
import {AuthLoadingState} from '@app/features/auth/flow/AuthLoadingState';
import {InviteHeader} from '@app/features/auth/flow/InviteHeader';
import {
	JOIN_COMMUNITY_DESCRIPTOR,
	NO_DESCRIPTION_PROVIDED_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import styles from '@app/features/invite/components/modals/InviteAcceptModal.module.css';
import Invites from '@app/features/invite/state/Invites';
import {
	isGroupDmInvite,
	isGuildInvite,
	isPackInvite as isPackInviteGuard,
} from '@app/features/invite/types/InviteTypes';
import {getGroupDmInviteCounts} from '@app/features/invite/utils/GroupDmInviteCounts';
import {
	GuildInvitePrimaryAction,
	getGuildInviteActionState,
	getGuildInvitePrimaryAction,
	isGuildInviteActionDisabled,
} from '@app/features/invite/utils/GuildInviteActionState';
import {
	ACCEPT_INVITE_DESCRIPTOR,
	INVITE_NOT_FOUND_DESCRIPTION_DESCRIPTOR,
	INVITE_NOT_FOUND_TITLE_DESCRIPTOR,
	INVITES_PAUSED_TRY_AGAIN_DESCRIPTOR,
	RAID_INVITES_PAUSED_SHORT_DESCRIPTOR,
} from '@app/features/invite/utils/InviteMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import foodPatternUrl from '@app/media/images/i-like-food.svg';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useState} from 'react';

const JOIN_GROUP_DM_DESCRIPTOR = msg({
	message: 'Join group DM',
	comment: 'Button or menu action label in the invite accept modal. Keep it concise.',
});
const INVITES_PAUSED_DESCRIPTOR = msg({
	message: 'Invites paused',
	comment: 'Button or menu action label in the invite accept modal. Keep it concise.',
});
const GO_TO_COMMUNITY_DESCRIPTOR = msg({
	message: 'Go to community',
	comment: 'Short label in the invite accept modal. Keep it concise.',
});
const EMOJI_PACK_DESCRIPTOR = msg({
	message: 'Emoji pack',
	comment: 'Short label in the invite accept modal. Keep it concise.',
});
const STICKER_PACK_DESCRIPTOR = msg({
	message: 'Sticker pack',
	comment: 'Short label in the invite accept modal. Keep it concise.',
});
const INSTALL_EMOJI_PACK_DESCRIPTOR = msg({
	message: 'Install emoji pack',
	comment: 'Short label in the invite accept modal. Keep it concise.',
});
const INSTALL_STICKER_PACK_DESCRIPTOR = msg({
	message: 'Install sticker pack',
	comment: 'Short label in the invite accept modal. Keep it concise.',
});
const CREATED_BY_DESCRIPTOR = msg({
	message: 'Created by {userName}',
	comment: 'Metadata label for an expression pack invite. userName is the pack creator username.',
});
const INVITED_BY_DESCRIPTOR = msg({
	message: 'Invited by {userTag}',
	comment: 'Metadata label for an expression pack invite. userTag is the inviter username and discriminator.',
});
const ACCEPTING_INVITE_INSTALLS_PACK_DESCRIPTOR = msg({
	message: 'Accepting this invite installs the pack automatically.',
	comment: 'Note shown on expression pack invites before accepting.',
});
const logger = new Logger('InviteAcceptModal');

interface InviteAcceptModalProps {
	code: string;
}

export const InviteAcceptModal = observer(function InviteAcceptModal({code}: InviteAcceptModalProps) {
	const {i18n} = useLingui();
	const inviteState = Invites.invites.get(code) ?? null;
	const invite = inviteState?.data ?? null;
	const [isAccepting, setIsAccepting] = useState(false);
	useEffect(() => {
		if (!inviteState) {
			void InviteCommands.fetchWithCoalescing(code).catch(() => {});
		}
	}, [code, inviteState]);
	const isGroupDM = invite != null && isGroupDmInvite(invite);
	const groupDMCounts =
		invite && isGroupDM
			? getGroupDmInviteCounts({
					channelId: invite.channel.id,
					inviteMemberCount: invite.member_count,
				})
			: null;
	const isPackInvite = invite != null && isPackInviteGuard(invite);
	const guildActionState = getGuildInviteActionState({invite});
	const {presenceCount, memberCount} = guildActionState;
	const inviteForHeader = useMemo(() => {
		if (!invite) return null;
		if (isGroupDM && groupDMCounts) {
			return {
				...invite,
				member_count: groupDMCounts.memberCount,
			};
		}
		return {
			...invite,
			presence_count: presenceCount,
			member_count: memberCount,
		};
	}, [invite, isGroupDM, presenceCount, memberCount, groupDMCounts?.memberCount]);
	const splashUrl = useMemo(() => {
		if (!invite || !isGuildInvite(invite)) {
			return null;
		}
		const guild = invite.guild;
		if (!guild.id || !guild.splash) {
			return null;
		}
		return AvatarUtils.getGuildSplashURL({
			id: guild.id,
			splash: guild.splash,
		});
	}, [invite]);
	const isJoinDisabled = isGuildInviteActionDisabled(guildActionState);
	const primaryActionType = getGuildInvitePrimaryAction(guildActionState);
	const primaryLabel = useMemo(() => {
		if (isGroupDM) return i18n._(JOIN_GROUP_DM_DESCRIPTOR);
		switch (primaryActionType) {
			case GuildInvitePrimaryAction.InvitesDisabled:
				return i18n._(INVITES_PAUSED_DESCRIPTOR);
			case GuildInvitePrimaryAction.GoToCommunity:
				return i18n._(GO_TO_COMMUNITY_DESCRIPTOR);
			default:
				return i18n._(JOIN_COMMUNITY_DESCRIPTOR);
		}
	}, [i18n.locale, isGroupDM, primaryActionType]);
	const handleDismiss = useCallback(() => {
		ModalCommands.pop();
	}, []);
	const handleAccept = useCallback(async () => {
		setIsAccepting(true);
		try {
			await InviteCommands.acceptAndTransitionToChannel(code, i18n);
			ModalCommands.pop();
		} catch (error) {
			logger.error(' Failed to accept invite:', error);
			setIsAccepting(false);
		}
	}, [code, i18n]);
	const renderBody = () => {
		if (!inviteState || inviteState.loading) {
			return (
				<div className={styles.stateHost} data-flx="invite.invite-accept-modal.render-body.state-host">
					<AuthLoadingState data-flx="invite.invite-accept-modal.render-body.auth-loading-state" />
				</div>
			);
		}
		if (inviteState.error || !inviteState.data || !inviteForHeader) {
			return (
				<div className={styles.stateHost} data-flx="invite.invite-accept-modal.render-body.state-host--2">
					<AuthErrorState
						title={i18n._(INVITE_NOT_FOUND_TITLE_DESCRIPTOR)}
						text={i18n._(INVITE_NOT_FOUND_DESCRIPTION_DESCRIPTOR)}
						data-flx="invite.invite-accept-modal.render-body.auth-error-state"
					/>
				</div>
			);
		}
		if (isPackInvite && invite) {
			const packKindLabel =
				invite.pack.type === 'emoji' ? i18n._(EMOJI_PACK_DESCRIPTOR) : i18n._(STICKER_PACK_DESCRIPTOR);
			const packActionLabel =
				invite.pack.type === 'emoji' ? i18n._(INSTALL_EMOJI_PACK_DESCRIPTOR) : i18n._(INSTALL_STICKER_PACK_DESCRIPTOR);
			const creatorUserName = NicknameUtils.getDisplayName(invite.pack.creator);
			const inviterTag = invite.inviter ? `${invite.inviter.username}#${invite.inviter.discriminator}` : null;
			return (
				<div className={styles.cardInner} data-flx="invite.invite-accept-modal.render-body.card-inner">
					<InviteHeader invite={inviteForHeader} data-flx="invite.invite-accept-modal.render-body.invite-header" />
					<p
						className={styles.packDescriptionText}
						data-flx="invite.invite-accept-modal.render-body.pack-description-text"
					>
						{invite.pack.description || i18n._(NO_DESCRIPTION_PROVIDED_DESCRIPTOR)}
					</p>
					<div className={styles.packMetaRow} data-flx="invite.invite-accept-modal.render-body.pack-meta-row">
						<span className={styles.packMetaText} data-flx="invite.invite-accept-modal.render-body.pack-meta-text">
							{packKindLabel}
						</span>
						<span className={styles.packMetaText} data-flx="invite.invite-accept-modal.render-body.pack-meta-text--2">
							{i18n._(CREATED_BY_DESCRIPTOR, {userName: creatorUserName})}
						</span>
						{inviterTag ? (
							<span className={styles.packMetaText} data-flx="invite.invite-accept-modal.render-body.pack-meta-text--3">
								{i18n._(INVITED_BY_DESCRIPTOR, {userTag: inviterTag})}
							</span>
						) : null}
					</div>
					<p className={styles.packNote} data-flx="invite.invite-accept-modal.render-body.pack-note">
						{i18n._(ACCEPTING_INVITE_INSTALLS_PACK_DESCRIPTOR)}
					</p>
					<div className={styles.actions} data-flx="invite.invite-accept-modal.render-body.actions">
						<Button
							onClick={handleAccept}
							disabled={isAccepting}
							submitting={isAccepting}
							data-flx="invite.invite-accept-modal.render-body.button.accept"
						>
							{packActionLabel}
						</Button>
					</div>
				</div>
			);
		}
		return (
			<div className={styles.cardInner} data-flx="invite.invite-accept-modal.render-body.card-inner--2">
				<InviteHeader invite={inviteForHeader} data-flx="invite.invite-accept-modal.render-body.invite-header--2" />
				{isJoinDisabled ? (
					<p className={styles.disabledText} data-flx="invite.invite-accept-modal.render-body.disabled-text">
						{guildActionState.isRaidDetected
							? i18n._(RAID_INVITES_PAUSED_SHORT_DESCRIPTOR, {productName: PRODUCT_NAME})
							: i18n._(INVITES_PAUSED_TRY_AGAIN_DESCRIPTOR)}
					</p>
				) : null}
				<div className={styles.actions} data-flx="invite.invite-accept-modal.render-body.actions--2">
					<Button
						onClick={handleAccept}
						disabled={isAccepting || isJoinDisabled}
						submitting={isAccepting}
						data-flx="invite.invite-accept-modal.render-body.button.accept--2"
					>
						{primaryLabel}
					</Button>
				</div>
			</div>
		);
	};
	return (
		<Modal.Root
			size="large"
			className={styles.root}
			centered
			onClose={handleDismiss}
			data-flx="invite.invite-accept-modal.root"
		>
			<Modal.ScreenReaderLabel
				text={i18n._(ACCEPT_INVITE_DESCRIPTOR)}
				data-flx="invite.invite-accept-modal.modal-screen-reader-label"
			/>
			<Modal.InsetCloseButton
				onClick={handleDismiss}
				disabled={isAccepting}
				data-flx="invite.invite-accept-modal.modal-inset-close-button.dismiss"
			/>
			<div className={styles.background} aria-hidden data-flx="invite.invite-accept-modal.background">
				{splashUrl ? (
					<div
						className={styles.splashImage}
						style={{backgroundImage: `url(${splashUrl})`}}
						data-flx="invite.invite-accept-modal.splash-image"
					/>
				) : (
					<div
						className={styles.patternImage}
						style={{backgroundImage: `url(${foodPatternUrl})`}}
						data-flx="invite.invite-accept-modal.pattern-image"
					/>
				)}
			</div>
			<div className={styles.cardHost} data-flx="invite.invite-accept-modal.card-host">
				<div className={styles.card} data-flx="invite.invite-accept-modal.card">
					{renderBody()}
				</div>
			</div>
		</Modal.Root>
	);
});
