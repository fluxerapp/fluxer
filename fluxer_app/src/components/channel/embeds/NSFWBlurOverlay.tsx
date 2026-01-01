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
import {WarningCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type {FC} from 'react';
import {ExternalLink} from '~/components/common/ExternalLink';
import GeoIPStore from '~/stores/GeoIPStore';
import {NSFWGateReason} from '~/stores/GuildNSFWAgreeStore';
import {formatRegion} from '~/utils/GeoUtils';
import * as HelpCenterUtils from '~/utils/HelpCenterUtils';
import styles from './NSFWBlurOverlay.module.css';

interface NSFWBlurOverlayProps {
	reason: NSFWGateReason;
}

export const NSFWBlurOverlay: FC<NSFWBlurOverlayProps> = observer(({reason}) => {
	const {i18n} = useLingui();
	const getMessage = () => {
		const countryCode = GeoIPStore.countryCode;
		const regionCode = GeoIPStore.regionCode;
		const regionName = formatRegion(i18n, countryCode, regionCode);

		switch (reason) {
			case NSFWGateReason.GEO_RESTRICTED:
				return (
					<Trans>Due to age verification laws in {regionName}, you cannot access NSFW content from this region.</Trans>
				);
			case NSFWGateReason.AGE_RESTRICTED:
				return (
					<Trans>
						You must be 18 or older to view this content.{' '}
						<ExternalLink href={HelpCenterUtils.getURL('1426347609450086400')}>Learn more</ExternalLink>
					</Trans>
				);
			default:
				return null;
		}
	};

	const message = getMessage();
	if (!message) return null;

	return (
		<div className={styles.warningContainer} style={{color: '#ff9933'}}>
			<WarningCircleIcon size={16} weight="fill" className={styles.warningIcon} />
			<span className={styles.warningText}>{message}</span>
		</div>
	);
});
