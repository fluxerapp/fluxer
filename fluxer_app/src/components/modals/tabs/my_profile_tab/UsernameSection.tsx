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

import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as PremiumModalActionCreators from '@app/actions/PremiumModalActionCreators';
import {FluxerTagChangeModal} from '@app/components/modals/FluxerTagChangeModal';
import styles from '@app/components/modals/tabs/my_profile_tab/UsernameSection.module.css';
import {Button} from '@app/components/uikit/button/Button';
import {Tooltip} from '@app/components/uikit/tooltip/Tooltip';
import type {UserRecord} from '@app/records/UserRecord';
import {LimitResolver} from '@app/utils/limits/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/utils/limits/LimitUtils';
import {shouldShowPremiumFeatures} from '@app/utils/PremiumUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CrownIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

interface UsernameSectionProps {
	isClaimed: boolean;
	user: UserRecord;
}

export const UsernameSection = observer(({isClaimed, user}: UsernameSectionProps) => {
	const {t} = useLingui();

	const hasCustomDiscriminator = isLimitToggleEnabled(
		{feature_custom_discriminator: LimitResolver.resolve({key: 'feature_custom_discriminator', fallback: 0})},
		'feature_custom_discriminator',
	);

	return (
		<div>
			<div className={styles.label}>
				<Trans>Username</Trans>
			</div>

			<div className={styles.actions}>
				{!isClaimed ? (
					<Tooltip text={t(msg`Claim your account to change your FluxerTag`)}>
						<div>
							<Button variant="primary" small disabled>
								<Trans>Change FluxerTag</Trans>
							</Button>
						</div>
					</Tooltip>
				) : (
					<Button
						variant="primary"
						small
						onClick={() => ModalActionCreators.push(modal(() => <FluxerTagChangeModal user={user} />))}
					>
						<Trans>Change FluxerTag</Trans>
					</Button>
				)}

				{!hasCustomDiscriminator && shouldShowPremiumFeatures() && (
					<Tooltip text={t(msg`Customize your 4-digit tag (#${user.discriminator}) to your liking with Plutonium`)}>
						<button
							type="button"
							onClick={() => {
								PremiumModalActionCreators.open();
							}}
							className={styles.premiumButton}
							aria-label={t(msg`Get Plutonium to customize your tag`)}
						>
							<CrownIcon weight="fill" size={18} />
						</button>
					</Tooltip>
				)}
			</div>

			<div className={styles.description}>
				<Trans>Change your username and 4-digit tag</Trans>
			</div>
		</div>
	);
});
