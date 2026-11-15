// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {getPremiumProductName, shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {FluxerTagChangeModal} from '@app/features/user/components/modals/FluxerTagChangeModal';
import styles from '@app/features/user/components/modals/tabs/my_profile_tab/MyProfileTabUsernameSection.module.css';
import type {User} from '@app/features/user/models/User';
import {accountGateReason} from '@app/features/user/utils/AccountGatePresentation';
import type {AccountGateResult} from '@app/features/user/utils/AccountGates';
import {hasUserTrait, UserTraits} from '@fluxer/constants/src/UserTraitConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {AtomIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const CUSTOMIZE_YOUR_TAG_TO_YOUR_LIKING_WITH_DESCRIPTOR = msg({
	message: 'Customize your tag ({discriminatorLabel}) to your liking with {premiumProductName}',
	comment:
		'Description text in the username section. Preserve {discriminatorLabel}, {premiumProductName}; they are inserted by code. Keep the tone plain and specific.',
});
const GET_TO_CUSTOMIZE_YOUR_TAG_DESCRIPTOR = msg({
	message: 'Get {premiumProductName} to customize your tag',
	comment:
		'Label in the username section. Preserve {premiumProductName}; it is inserted by code. Keep the tone plain and specific.',
});
const USERNAME_FROM_SIGN_IN_PROVIDER_DESCRIPTOR = msg({
	message: 'Your username comes from your sign-in provider.',
	comment:
		'Tooltip on the disabled Change username button when the sign-in provider sets usernames. Keep the tone plain and specific.',
});

function signInProviderSetsUsername(user: User): boolean {
	const sso = RuntimeConfig.getSnapshot().sso;
	return (
		RuntimeConfig.usesUsernameSignIn &&
		sso?.enabled === true &&
		sso.username_from_provider &&
		!user.bot &&
		hasUserTrait(user.traits, UserTraits.SSO)
	);
}

function usernameChangeBlockedReason(i18n: I18n, user: User, profileGate: AccountGateResult): string | null {
	if (signInProviderSetsUsername(user)) return i18n._(USERNAME_FROM_SIGN_IN_PROVIDER_DESCRIPTOR);
	if (!profileGate.allowed) return accountGateReason(i18n, profileGate);
	return null;
}

interface UsernameSectionProps {
	profileGate: AccountGateResult;
	user: User;
}

export const UsernameSection = observer(({profileGate, user}: UsernameSectionProps) => {
	const {i18n} = useLingui();
	const hasCustomDiscriminator = isLimitToggleEnabled(
		{feature_custom_discriminator: LimitResolver.resolve({key: 'feature_custom_discriminator', fallback: 0})},
		'feature_custom_discriminator',
	);
	const discriminatorLabel = `#${user.discriminator}`;
	const disabledReason = usernameChangeBlockedReason(i18n, user, profileGate);
	return (
		<div data-flx="user.my-profile-tab.username-section.div">
			<div className={styles.label} data-flx="user.my-profile-tab.username-section.label">
				<Trans>Username</Trans>
			</div>
			<div className={styles.actions} data-flx="user.my-profile-tab.username-section.actions">
				{disabledReason !== null ? (
					<Tooltip text={disabledReason} data-flx="user.my-profile-tab.username-section.tooltip">
						<div data-flx="user.my-profile-tab.username-section.div--2">
							<Button variant="primary" small disabled data-flx="user.my-profile-tab.username-section.button">
								<Trans>Change username</Trans>
							</Button>
						</div>
					</Tooltip>
				) : (
					<Button
						variant="primary"
						small
						onClick={() =>
							ModalCommands.push(
								modal(() => (
									<FluxerTagChangeModal
										user={user}
										data-flx="user.my-profile-tab.username-section.fluxer-tag-change-modal"
									/>
								)),
							)
						}
						data-flx="user.my-profile-tab.username-section.button.push"
					>
						<Trans>Change username</Trans>
					</Button>
				)}
				{!RuntimeConfig.usesUniqueUsernames && !hasCustomDiscriminator && shouldShowPremiumFeatures() && (
					<Tooltip
						text={i18n._(CUSTOMIZE_YOUR_TAG_TO_YOUR_LIKING_WITH_DESCRIPTOR, {
							discriminatorLabel,
							premiumProductName: getPremiumProductName(),
						})}
						data-flx="user.my-profile-tab.username-section.tooltip--3"
					>
						<button
							type="button"
							onClick={() => {
								PremiumModalCommands.open();
							}}
							className={styles.premiumButton}
							aria-label={i18n._(GET_TO_CUSTOMIZE_YOUR_TAG_DESCRIPTOR, {premiumProductName: getPremiumProductName()})}
							data-flx="user.my-profile-tab.username-section.premium-button.open"
						>
							<AtomIcon weight="fill" size={remFromPx(18)} data-flx="user.my-profile-tab.username-section.atom-icon" />
						</button>
					</Tooltip>
				)}
			</div>
		</div>
	);
});
