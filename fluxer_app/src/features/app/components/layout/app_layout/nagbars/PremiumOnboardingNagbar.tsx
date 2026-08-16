// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {PREMIUM_PRODUCT_FULL_NAME, PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as NagbarCommands from '@app/features/ui/commands/NagbarCommands';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import Users from '@app/features/user/state/Users';
import {getFormattedFullDate} from '@app/features/user/utils/DateFormatting';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo} from 'react';

const PREMIUM_ONBOARDING_DEFAULT_MESSAGE_DESCRIPTOR = msg({
	message: 'Welcome to {premiumProductFullName}. Explore your {premiumProductName} perks and manage your subscription.',
	comment:
		'Premium onboarding nagbar body. {premiumProductFullName} is the full premium product name, such as Fluxer Plutonium. {premiumProductName} is the short product name.',
});
const PREMIUM_ONBOARDING_VISIONARY_MESSAGE_DESCRIPTOR = msg({
	message:
		'Welcome to {premiumProductFullName}. You have lifetime access as a Visionary. Explore your {premiumProductName} perks.',
	comment:
		'Premium onboarding nagbar body for lifetime premium users. Visionary is the lifetime premium tier name. {premiumProductFullName} is the full premium product name. {premiumProductName} is the short product name.',
});
const PREMIUM_ONBOARDING_CANCELING_MESSAGE_DESCRIPTOR = msg({
	message:
		'Welcome to {premiumProductFullName}. Your subscription is active until {formattedDate}. Explore your {premiumProductName} perks and manage your subscription.',
	comment:
		'Premium onboarding nagbar body for a subscription that will cancel. {premiumProductFullName} is the full premium product name, {premiumProductName} is the short product name, and {formattedDate} is already localized.',
});
const PREMIUM_ONBOARDING_MONTHLY_MESSAGE_DESCRIPTOR = msg({
	message:
		'Welcome to {premiumProductFullName}. Your monthly subscription renews on {formattedDate}. Explore your {premiumProductName} perks and manage your subscription.',
	comment:
		'Premium onboarding nagbar body for a monthly subscription. {premiumProductFullName} is the full premium product name, {premiumProductName} is the short product name, and {formattedDate} is already localized.',
});
const PREMIUM_ONBOARDING_YEARLY_MESSAGE_DESCRIPTOR = msg({
	message:
		'Welcome to {premiumProductFullName}. Your yearly subscription renews on {formattedDate}. Explore your {premiumProductName} perks and manage your subscription.',
	comment:
		'Premium onboarding nagbar body for a yearly subscription. {premiumProductFullName} is the full premium product name, {premiumProductName} is the short product name, and {formattedDate} is already localized.',
});
const PREMIUM_ONBOARDING_GIFT_MESSAGE_DESCRIPTOR = msg({
	message:
		'Welcome to {premiumProductFullName}. Your gift subscription runs until {formattedDate}. Explore your {premiumProductName} perks.',
	comment:
		'Premium onboarding nagbar body for a gift subscription. {premiumProductFullName} is the full premium product name, {premiumProductName} is the short product name, and {formattedDate} is already localized.',
});
const VIEW_PREMIUM_FEATURES_DESCRIPTOR = msg({
	message: 'View {premiumProductName} features',
	comment:
		'Button label on the premium onboarding nagbar. Opens premium feature and subscription settings. Preserve {premiumProductName}; it is inserted by code.',
});

function useOnboardingMessage(i18n: I18n): string {
	const user = Users.currentUser;
	return useMemo(() => {
		const premiumProductFullName = PREMIUM_PRODUCT_FULL_NAME;
		const premiumProductName = PREMIUM_PRODUCT_NAME;
		if (!user) {
			return i18n._(PREMIUM_ONBOARDING_DEFAULT_MESSAGE_DESCRIPTOR, {premiumProductFullName, premiumProductName});
		}
		const isVisionary = user.premiumType === UserPremiumTypes.LIFETIME;
		const billingCycle = user.premiumBillingCycle;
		const premiumUntil = user.premiumUntil;
		const willCancel = user.premiumWillCancel;
		if (isVisionary) {
			return i18n._(PREMIUM_ONBOARDING_VISIONARY_MESSAGE_DESCRIPTOR, {premiumProductFullName, premiumProductName});
		}
		if (willCancel && premiumUntil) {
			const formattedDate = getFormattedFullDate(premiumUntil);
			return i18n._(PREMIUM_ONBOARDING_CANCELING_MESSAGE_DESCRIPTOR, {
				premiumProductFullName,
				premiumProductName,
				formattedDate,
			});
		}
		if (billingCycle && premiumUntil) {
			const formattedDate = getFormattedFullDate(premiumUntil);
			if (billingCycle === 'monthly') {
				return i18n._(PREMIUM_ONBOARDING_MONTHLY_MESSAGE_DESCRIPTOR, {
					premiumProductFullName,
					premiumProductName,
					formattedDate,
				});
			}
			return i18n._(PREMIUM_ONBOARDING_YEARLY_MESSAGE_DESCRIPTOR, {
				premiumProductFullName,
				premiumProductName,
				formattedDate,
			});
		}
		if (premiumUntil) {
			const formattedDate = getFormattedFullDate(premiumUntil);
			return i18n._(PREMIUM_ONBOARDING_GIFT_MESSAGE_DESCRIPTOR, {
				premiumProductFullName,
				premiumProductName,
				formattedDate,
			});
		}
		return i18n._(PREMIUM_ONBOARDING_DEFAULT_MESSAGE_DESCRIPTOR, {premiumProductFullName, premiumProductName});
	}, [i18n.locale, user]);
}

export const PremiumOnboardingNagbar = observer(function PremiumOnboardingNagbar({isMobile}: {isMobile: boolean}) {
	const {i18n} = useLingui();
	const message = useOnboardingMessage(i18n);
	const handleOpenPremiumSettings = useCallback(() => {
		NagbarCommands.dismissNagbar('premiumOnboardingDismissed');
		void UserCommands.update({has_dismissed_premium_onboarding: true});
		ModalCommands.push(
			modal(() => (
				<UserSettingsModal
					initialTab="plutonium"
					data-flx="app.app-layout.nagbars.premium-onboarding-nagbar.handle-open-premium-settings.user-settings-modal"
				/>
			)),
		);
	}, []);
	const handleDismiss = useCallback(() => {
		NagbarCommands.dismissNagbar('premiumOnboardingDismissed');
		void UserCommands.update({has_dismissed_premium_onboarding: true});
	}, []);
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.BRAND].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.BRAND].textColor}
			dismissible
			onDismiss={handleDismiss}
			data-flx="app.app-layout.nagbars.premium-onboarding-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={handleDismiss}
				message={message}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={handleOpenPremiumSettings}
						data-flx="app.app-layout.nagbars.premium-onboarding-nagbar.nagbar-button.open-premium-settings"
					>
						{i18n._(VIEW_PREMIUM_FEATURES_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.premium-onboarding-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
