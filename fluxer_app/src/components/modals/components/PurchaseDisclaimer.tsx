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
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {ExternalLink} from '~/components/common/ExternalLink';
import {Routes} from '~/Routes';
import styles from './PurchaseDisclaimer.module.css';

export const PurchaseDisclaimer = observer(
	({isPremium = false, align = 'center'}: {isPremium?: boolean; align?: 'left' | 'center'}) => (
		<p className={clsx(styles.disclaimer, align === 'center' ? styles.center : styles.left)}>
			{isPremium ? (
				<Trans>
					By purchasing, you agreed to our <ExternalLink href={Routes.terms()}>Terms of Service</ExternalLink> and{' '}
					<ExternalLink href={Routes.privacy()}>Privacy Policy</ExternalLink>. All purchases are refundable within 14
					days by emailing <ExternalLink href="mailto:support@fluxer.app">support@fluxer.app</ExternalLink>. Chargebacks
					result in permanent account bans — if you want a refund, we're more than happy to give you one if you contact
					us first! Payment information is securely handled by Stripe — we never have access to your full card number.
				</Trans>
			) : (
				<Trans>
					By purchasing, you agree to our <ExternalLink href={Routes.terms()}>Terms of Service</ExternalLink> and{' '}
					<ExternalLink href={Routes.privacy()}>Privacy Policy</ExternalLink>. All purchases are refundable within 14
					days by emailing <ExternalLink href="mailto:support@fluxer.app">support@fluxer.app</ExternalLink>. Chargebacks
					result in permanent account bans — if you want a refund, we're more than happy to give you one if you contact
					us first! Payment information is securely handled by Stripe — we never have access to your full card number.
				</Trans>
			)}
		</p>
	),
);
