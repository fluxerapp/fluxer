// SPDX-License-Identifier: AGPL-3.0-or-later

import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {FluxerTagChangeModal} from '@app/features/user/components/modals/FluxerTagChangeModal';
import styles from '@app/features/user/components/modals/tabs/my_profile_tab/MyProfileTabUsernameSection.module.css';
import type {User} from '@app/features/user/models/User';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CrownIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const CLAIM_YOUR_ACCOUNT_TO_CHANGE_YOUR_USERNAME_DESCRIPTOR = msg({
	message: 'Claim your account to change your username',
	comment: 'Label in the username section.',
});
const VERIFY_YOUR_EMAIL_TO_CHANGE_YOUR_USERNAME_DESCRIPTOR = msg({
	message: 'Verify your email to change your username',
	comment: 'Label in the username section.',
});
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

interface UsernameSectionProps {
	isClaimed: boolean;
	isEmailVerified: boolean;
	user: User;
}

export const UsernameSection = observer(({isClaimed, isEmailVerified, user}: UsernameSectionProps) => {
	const {i18n} = useLingui();
	const hasCustomDiscriminator = isLimitToggleEnabled(
		{feature_custom_discriminator: LimitResolver.resolve({key: 'feature_custom_discriminator', fallback: 0})},
		'feature_custom_discriminator',
	);
	const discriminatorLabel = `#${user.discriminator}`;
	return (
		<div data-flx="user.my-profile-tab.username-section.div">
			<div className={styles.label} data-flx="user.my-profile-tab.username-section.label">
				<Trans>Username</Trans>
			</div>
			<div className={styles.actions} data-flx="user.my-profile-tab.username-section.actions">
				{!isClaimed ? (
					<Tooltip
						text={i18n._(CLAIM_YOUR_ACCOUNT_TO_CHANGE_YOUR_USERNAME_DESCRIPTOR)}
						data-flx="user.my-profile-tab.username-section.tooltip"
					>
						<div data-flx="user.my-profile-tab.username-section.div--2">
							<Button variant="primary" small disabled data-flx="user.my-profile-tab.username-section.button">
								<Trans>Change username</Trans>
							</Button>
						</div>
					</Tooltip>
				) : !isEmailVerified ? (
					<Tooltip
						text={i18n._(VERIFY_YOUR_EMAIL_TO_CHANGE_YOUR_USERNAME_DESCRIPTOR)}
						data-flx="user.my-profile-tab.username-section.tooltip--2"
					>
						<div data-flx="user.my-profile-tab.username-section.div--3">
							<Button variant="primary" small disabled data-flx="user.my-profile-tab.username-section.button--2">
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
				{!hasCustomDiscriminator && shouldShowPremiumFeatures() && (
					<Tooltip
						text={i18n._(CUSTOMIZE_YOUR_TAG_TO_YOUR_LIKING_WITH_DESCRIPTOR, {
							discriminatorLabel,
							premiumProductName: PREMIUM_PRODUCT_NAME,
						})}
						data-flx="user.my-profile-tab.username-section.tooltip--3"
					>
						<button
							type="button"
							onClick={() => {
								PremiumModalCommands.open();
							}}
							className={styles.premiumButton}
							aria-label={i18n._(GET_TO_CUSTOMIZE_YOUR_TAG_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
							data-flx="user.my-profile-tab.username-section.premium-button.open"
						>
							<CrownIcon
								weight="fill"
								size={remFromPx(18)}
								data-flx="user.my-profile-tab.username-section.crown-icon"
							/>
						</button>
					</Tooltip>
				)}
			</div>
		</div>
	);
});
