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
import * as InviteActionCreators from '~/actions/InviteActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {AuthErrorState} from '~/components/auth/AuthErrorState';
import {AuthLoadingState} from '~/components/auth/AuthLoadingState';
import {InviteHeader} from '~/components/auth/InviteHeader';
import styles from '~/components/modals/InviteAcceptModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import foodPatternUrl from '~/images/i-like-food.svg';
import InviteStore from '~/stores/InviteStore';
import {isGroupDmInvite, isGuildInvite, isPackInvite as isPackInviteGuard} from '~/types/InviteTypes';
import * as AvatarUtils from '~/utils/AvatarUtils';
import {
	GuildInvitePrimaryAction,
	getGuildInviteActionState,
	getGuildInvitePrimaryAction,
	isGuildInviteActionDisabled,
} from '~/utils/invite/GuildInviteActionState';

interface InviteAcceptModalProps {
	code: string;
}

export const InviteAcceptModal = observer(function InviteAcceptModal({code}: InviteAcceptModalProps) {
	const {t, i18n} = useLingui();
	const inviteState = InviteStore.invites.get(code) ?? null;
	const invite = inviteState?.data ?? null;

	const [isAccepting, setIsAccepting] = React.useState(false);

	React.useEffect(() => {
		if (!inviteState) {
			void InviteActionCreators.fetchWithCoalescing(code).catch(() => {});
		}
	}, [code, inviteState]);

	const isGroupDM = invite != null && isGroupDmInvite(invite);
	const isPackInvite = invite != null && isPackInviteGuard(invite);

	const guildActionState = getGuildInviteActionState({invite});
	const {presenceCount, memberCount} = guildActionState;

	const inviteForHeader = React.useMemo(() => {
		if (!invite) return null;
		return {
			...invite,
			presence_count: presenceCount,
			member_count: memberCount,
		};
	}, [invite, presenceCount, memberCount]);

	const splashUrl = React.useMemo(() => {
		if (!invite || !isGuildInvite(invite)) {
			return null;
		}
		const guild = invite.guild;
		if (!guild.id || !guild.splash) {
			return null;
		}
		return AvatarUtils.getGuildSplashURL(
			{
				id: guild.id,
				splash: guild.splash,
			},
			4096,
		);
	}, [invite]);

	const isJoinDisabled = isGuildInviteActionDisabled(guildActionState);
	const primaryActionType = getGuildInvitePrimaryAction(guildActionState);

	const primaryLabel = React.useMemo(() => {
		if (isGroupDM) return t`Join Group DM`;
		switch (primaryActionType) {
			case GuildInvitePrimaryAction.InvitesDisabled:
				return t`Invites Disabled`;
			case GuildInvitePrimaryAction.GoToCommunity:
				return t`Go to Community`;
			default:
				return t`Join Community`;
		}
	}, [isGroupDM, primaryActionType]);

	const handleDismiss = React.useCallback(() => {
		ModalActionCreators.pop();
	}, []);

	const handleAccept = React.useCallback(async () => {
		setIsAccepting(true);
		try {
			await InviteActionCreators.acceptAndTransitionToChannel(code, i18n);
			ModalActionCreators.pop();
		} catch (error) {
			console.error('[InviteAcceptModal] Failed to accept invite:', error);
			setIsAccepting(false);
		}
	}, [code]);

	const renderBody = () => {
		if (!inviteState || inviteState.loading) {
			return (
				<div className={styles.stateHost}>
					<AuthLoadingState />
				</div>
			);
		}

		if (inviteState.error || !inviteState.data || !inviteForHeader) {
			return (
				<div className={styles.stateHost}>
					<AuthErrorState
						title={<Trans>Invite not found</Trans>}
						text={<Trans>This invite may have expired or been deleted.</Trans>}
					/>
				</div>
			);
		}

		if (isPackInvite && invite) {
			const packKindLabel = invite.pack.type === 'emoji' ? t`Emoji pack` : t`Sticker pack`;
			const packActionLabel = invite.pack.type === 'emoji' ? t`Install Emoji Pack` : t`Install Sticker Pack`;

			return (
				<div className={styles.cardInner}>
					<InviteHeader invite={inviteForHeader} />
					<p className={styles.packDescriptionText}>{invite.pack.description || t`No description provided.`}</p>
					<div className={styles.packMetaRow}>
						<span className={styles.packMetaText}>{packKindLabel}</span>
						<span className={styles.packMetaText}>
							<Trans>Created by {invite.pack.creator.username}</Trans>
						</span>
						{invite.inviter ? (
							<span className={styles.packMetaText}>
								<Trans>
									Invited by {invite.inviter.username}#{invite.inviter.discriminator}
								</Trans>
							</span>
						) : null}
					</div>
					<p className={styles.packNote}>
						<Trans>Accepting this invite installs the pack automatically.</Trans>
					</p>
					<div className={styles.actions}>
						<Button onClick={handleAccept} disabled={isAccepting} submitting={isAccepting}>
							{packActionLabel}
						</Button>
					</div>
				</div>
			);
		}

		return (
			<div className={styles.cardInner}>
				<InviteHeader invite={inviteForHeader} />

				{isJoinDisabled ? (
					<p className={styles.disabledText}>
						<Trans>This community has temporarily disabled invites. You can try again later.</Trans>
					</p>
				) : null}

				<div className={styles.actions}>
					<Button onClick={handleAccept} disabled={isAccepting || isJoinDisabled} submitting={isAccepting}>
						{primaryLabel}
					</Button>
				</div>
			</div>
		);
	};

	return (
		<Modal.Root size="large" className={styles.root} centered onClose={handleDismiss}>
			<Modal.ScreenReaderLabel text={t`Accept invite`} />
			<Modal.InsetCloseButton onClick={handleDismiss} disabled={isAccepting} />

			<div className={styles.background} aria-hidden>
				{splashUrl ? (
					<div className={styles.splashImage} style={{backgroundImage: `url(${splashUrl})`}} />
				) : (
					<div className={styles.patternImage} style={{backgroundImage: `url(${foodPatternUrl})`}} />
				)}
			</div>

			<div className={styles.cardHost}>
				<div className={styles.card}>{renderBody()}</div>
			</div>
		</Modal.Root>
	);
});
