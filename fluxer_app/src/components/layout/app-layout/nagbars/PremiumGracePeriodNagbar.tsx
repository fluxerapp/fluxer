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

import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as NagbarActionCreators from '~/actions/NagbarActionCreators';
import * as PremiumActionCreators from '~/actions/PremiumActionCreators';
import {Nagbar} from '~/components/layout/Nagbar';
import {NagbarButton} from '~/components/layout/NagbarButton';
import {NagbarContent} from '~/components/layout/NagbarContent';
import UserStore from '~/stores/UserStore';
import * as LocaleUtils from '~/utils/LocaleUtils';
import {openExternalUrl} from '~/utils/NativeUtils';

export const PremiumGracePeriodNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const user = UserStore.currentUser;
	const [loadingPortal, setLoadingPortal] = React.useState(false);

	const handleOpenCustomerPortal = async () => {
		setLoadingPortal(true);
		try {
			const url = await PremiumActionCreators.createCustomerPortalSession();
			void openExternalUrl(url);
		} catch (error) {
			console.error('Failed to open customer portal', error);
		} finally {
			setLoadingPortal(false);
		}
	};

	const handleDismiss = () => {
		NagbarActionCreators.dismissNagbar('premiumGracePeriodDismissed');
	};

	if (!user?.premiumUntil || user?.premiumWillCancel) return null;

	const expiryDate = new Date(user.premiumUntil);
	const gracePeriodMs = 3 * 24 * 60 * 60 * 1000;
	const graceEndDate = new Date(expiryDate.getTime() + gracePeriodMs);
	const locale = LocaleUtils.getCurrentLocale();

	const formattedGraceDate = graceEndDate.toLocaleDateString(locale, {
		month: 'long',
		day: 'numeric',
		year: 'numeric',
	});

	return (
		<Nagbar isMobile={isMobile} backgroundColor="#f97316" textColor="#ffffff" dismissible onDismiss={handleDismiss}>
			<NagbarContent
				isMobile={isMobile}
				message={
					<Trans>
						Your subscription failed to renew, but you still have access to Plutonium perks until{' '}
						<strong>{formattedGraceDate}</strong>. Take action now or you'll lose all perks.
					</Trans>
				}
				actions={
					<>
						{isMobile && (
							<NagbarButton isMobile={isMobile} onClick={handleDismiss}>
								<Trans>Dismiss</Trans>
							</NagbarButton>
						)}
						<NagbarButton isMobile={isMobile} onClick={handleOpenCustomerPortal} disabled={loadingPortal}>
							{loadingPortal ? <Trans>Opening...</Trans> : <Trans>Manage Subscription</Trans>}
						</NagbarButton>
					</>
				}
			/>
		</Nagbar>
	);
});
