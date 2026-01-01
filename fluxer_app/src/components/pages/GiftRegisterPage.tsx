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
import {GiftIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect} from 'react';
import * as AuthenticationActionCreators from '~/actions/AuthenticationActionCreators';
import * as GiftActionCreators from '~/actions/GiftActionCreators';
import {AuthBottomLink} from '~/components/auth/AuthBottomLink';
import {AuthErrorState} from '~/components/auth/AuthErrorState';
import {AuthLoadingState} from '~/components/auth/AuthLoadingState';
import {AuthMinimalRegisterFormCore} from '~/components/auth/AuthMinimalRegisterFormCore';
import sharedStyles from '~/components/auth/AuthPageStyles.module.css';
import {DesktopDeepLinkPrompt} from '~/components/auth/DesktopDeepLinkPrompt';
import {GiftHeader} from '~/components/auth/GiftHeader';
import {useFluxerDocumentTitle} from '~/hooks/useFluxerDocumentTitle';
import {useParams} from '~/lib/router';
import {Routes} from '~/Routes';
import GiftStore from '~/stores/GiftStore';

const GiftRegisterPage = observer(function GiftRegisterPage() {
	const {t} = useLingui();
	const {code} = useParams() as {code: string};

	useFluxerDocumentTitle(t`Claim Gift`);

	const giftState = GiftStore.gifts.get(code) ?? null;

	const handleRegisterComplete = useCallback(
		async (response: {token: string; user_id: string}) => {
			await AuthenticationActionCreators.completeLogin({
				token: response.token,
				userId: response.user_id,
			});
			GiftActionCreators.openAcceptModal(code);
		},
		[code],
	);

	useEffect(() => {
		const currentGiftState = GiftStore.gifts.get(code) ?? null;
		if (!currentGiftState && code) {
			void GiftActionCreators.fetchWithCoalescing(code).catch(() => {});
		}
	}, [code]);

	if (!giftState || giftState.loading) {
		return <AuthLoadingState />;
	}

	if (giftState.error || !giftState.data) {
		return (
			<AuthErrorState
				title={<Trans>Gift not found</Trans>}
				text={<Trans>This gift code may be invalid, expired, or already redeemed.</Trans>}
			/>
		);
	}

	const gift = giftState.data;

	if (gift.redeemed) {
		return (
			<AuthErrorState
				icon={GiftIcon}
				title={<Trans>Gift already redeemed</Trans>}
				text={<Trans>This gift code has already been claimed.</Trans>}
			/>
		);
	}

	return (
		<>
			<DesktopDeepLinkPrompt code={code} kind="gift" />

			<GiftHeader gift={gift} variant="register" />

			<div className={sharedStyles.container}>
				<AuthMinimalRegisterFormCore
					submitLabel={<Trans>Create account to claim gift</Trans>}
					redirectPath="/"
					onRegister={handleRegisterComplete}
				/>

				<AuthBottomLink variant="login" to={Routes.giftLogin(code)} />
			</div>
		</>
	);
});

export default GiftRegisterPage;
