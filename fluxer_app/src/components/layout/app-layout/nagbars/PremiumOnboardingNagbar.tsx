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
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as UserActionCreators from '~/actions/UserActionCreators';
import {Nagbar} from '~/components/layout/Nagbar';
import {NagbarButton} from '~/components/layout/NagbarButton';
import {NagbarContent} from '~/components/layout/NagbarContent';
import {UserSettingsModal} from '~/components/modals/UserSettingsModal';

export const PremiumOnboardingNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const handleOpenPremiumSettings = () => {
		void UserActionCreators.update({has_dismissed_premium_onboarding: true});
		ModalActionCreators.push(modal(() => <UserSettingsModal initialTab="plutonium" />));
	};

	const handleDismiss = () => {
		void UserActionCreators.update({has_dismissed_premium_onboarding: true});
	};

	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor="var(--brand-primary)"
			textColor="var(--text-on-brand-primary)"
			dismissible
			onDismiss={handleDismiss}
		>
			<NagbarContent
				isMobile={isMobile}
				message={
					<Trans>Welcome to Fluxer Plutonium! Explore your premium features and manage your subscription.</Trans>
				}
				actions={
					<>
						{isMobile && (
							<NagbarButton isMobile={isMobile} onClick={handleDismiss}>
								<Trans>Dismiss</Trans>
							</NagbarButton>
						)}
						<NagbarButton isMobile={isMobile} onClick={handleOpenPremiumSettings}>
							<Trans>View Premium Features</Trans>
						</NagbarButton>
					</>
				}
			/>
		</Nagbar>
	);
});
