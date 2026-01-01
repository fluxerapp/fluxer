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

import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CrownIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as PremiumModalActionCreators from '~/actions/PremiumModalActionCreators';
import {FluxerTagChangeModal} from '~/components/modals/FluxerTagChangeModal';
import {Button} from '~/components/uikit/Button/Button';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import styles from './UsernameSection.module.css';

interface UsernameSectionProps {
	isClaimed: boolean;
	hasPremium: boolean;
	discriminator: string;
}

export const UsernameSection = observer(({isClaimed, hasPremium, discriminator}: UsernameSectionProps) => {
	const {t} = useLingui();

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
						onClick={() => ModalActionCreators.push(modal(() => <FluxerTagChangeModal />))}
					>
						<Trans>Change FluxerTag</Trans>
					</Button>
				)}

				{!hasPremium && (
					<Tooltip text={t(msg`Customize your 4-digit tag (#${discriminator}) to your liking with Plutonium`)}>
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
