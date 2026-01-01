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
import {useEffect} from 'react';
import * as InviteActionCreators from '~/actions/InviteActionCreators';
import {GuildFeatures, GuildSplashCardAlignment} from '~/Constants';
import {AuthBottomLink} from '~/components/auth/AuthBottomLink';
import {AuthErrorState} from '~/components/auth/AuthErrorState';
import {AuthLoadingState} from '~/components/auth/AuthLoadingState';
import {AuthMinimalRegisterFormCore} from '~/components/auth/AuthMinimalRegisterFormCore';
import sharedStyles from '~/components/auth/AuthPageStyles.module.css';
import {AuthRouterLink} from '~/components/auth/AuthRouterLink';
import {DesktopDeepLinkPrompt} from '~/components/auth/DesktopDeepLinkPrompt';
import {GuildInviteHeader, InviteHeader} from '~/components/auth/InviteHeader';
import {Button} from '~/components/uikit/Button/Button';
import {useAuthLayoutContext} from '~/contexts/AuthLayoutContext';
import {useFluxerDocumentTitle} from '~/hooks/useFluxerDocumentTitle';
import {useParams} from '~/lib/router';
import {Routes} from '~/Routes';
import InviteStore from '~/stores/InviteStore';
import {isGroupDmInvite, isGuildInvite} from '~/types/InviteTypes';
import * as AvatarUtils from '~/utils/AvatarUtils';

const InviteRegisterPage = observer(function InviteRegisterPage() {
	const {t} = useLingui();
	const {code} = useParams() as {code: string};
	const {setSplashUrl, setSplashCardAlignment} = useAuthLayoutContext();

	useFluxerDocumentTitle(t`Accept Invite`);

	const inviteState = InviteStore.invites.get(code) ?? null;
	const inviteData = inviteState?.data ?? null;
	const guildInvite = inviteData && isGuildInvite(inviteData) ? inviteData : null;

	useEffect(() => {
		const currentInviteState = InviteStore.invites.get(code) ?? null;
		if (!currentInviteState && code) {
			void InviteActionCreators.fetchWithCoalescing(code).catch(() => {});
		}
	}, [code]);

	useEffect(() => {
		if (!guildInvite) {
			return;
		}
		const guild = guildInvite.guild;
		if (guild?.splash) {
			const splashUrl = AvatarUtils.getGuildSplashURL(
				{
					id: guild.id,
					splash: guild.splash,
				},
				4096,
			);
			setSplashUrl(splashUrl);
		}
	}, [guildInvite?.guild?.splash, guildInvite?.guild?.id, setSplashUrl]);

	useEffect(() => {
		if (guildInvite) {
			setSplashCardAlignment(guildInvite.guild.splash_card_alignment ?? GuildSplashCardAlignment.CENTER);
		} else {
			setSplashCardAlignment(GuildSplashCardAlignment.CENTER);
		}
	}, [guildInvite?.guild?.splash_card_alignment, setSplashCardAlignment]);

	if (!inviteState || inviteState.loading) {
		return <AuthLoadingState />;
	}

	if (inviteState.error || !inviteState.data) {
		return (
			<AuthErrorState
				title={<Trans>Invite not found</Trans>}
				text={<Trans>This invite may have expired or been deleted.</Trans>}
			/>
		);
	}

	const invite = inviteState.data;
	const isGroupDM = isGroupDmInvite(invite);

	const guildFeatures = guildInvite?.guild.features
		? Array.isArray(guildInvite.guild.features)
			? guildInvite.guild.features
			: [...guildInvite.guild.features]
		: [];
	const isInvitesDisabled = guildFeatures.includes(GuildFeatures.INVITES_DISABLED);

	if (isInvitesDisabled && !isGroupDM) {
		return (
			<div className={sharedStyles.container}>
				<DesktopDeepLinkPrompt code={code} kind="invite" />

				{guildInvite ? <GuildInviteHeader invite={guildInvite} /> : null}

				<div className={sharedStyles.disabledContainer}>
					<p className={sharedStyles.disabledText}>
						<Trans>This community has temporarily disabled invites.</Trans>
					</p>
					<p className={sharedStyles.disabledSubtext}>
						<Trans>
							You can still create an account or log in. If invites are re-enabled later, you can use this same link to
							join.
						</Trans>
					</p>
				</div>

				<div className={sharedStyles.disabledActions}>
					<AuthRouterLink to="/register" className={sharedStyles.disabledActionLink}>
						<Button fitContainer>
							<Trans>Create Account</Trans>
						</Button>
					</AuthRouterLink>
					<AuthRouterLink to="/login" className={sharedStyles.disabledActionLink}>
						<Button fitContainer variant="secondary">
							<Trans>Log In</Trans>
						</Button>
					</AuthRouterLink>
				</div>
			</div>
		);
	}

	return (
		<>
			<DesktopDeepLinkPrompt code={code} kind="invite" />

			<InviteHeader invite={invite} />

			<div className={sharedStyles.container}>
				<AuthMinimalRegisterFormCore submitLabel={<Trans>Create account</Trans>} redirectPath="/" inviteCode={code} />

				<AuthBottomLink variant="login" to={Routes.inviteLogin(code)} />
			</div>
		</>
	);
});

export default InviteRegisterPage;
