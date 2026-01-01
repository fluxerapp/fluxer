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
import {WarningIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import * as GuildNSFWActionCreators from '~/actions/GuildNSFWActionCreators';
import {ExternalLink} from '~/components/common/ExternalLink';
import {Button} from '~/components/uikit/Button/Button';
import GeoIPStore from '~/stores/GeoIPStore';
import {NSFWGateReason} from '~/stores/GuildNSFWAgreeStore';
import {formatRegion} from '~/utils/GeoUtils';
import * as HelpCenterUtils from '~/utils/HelpCenterUtils';
import styles from './NSFWChannelGate.module.css';

interface Props {
	channelId: string;
	reason: NSFWGateReason;
}

export const NSFWChannelGate = observer(({channelId, reason}: Props) => {
	const {i18n} = useLingui();
	const handleAgree = () => {
		GuildNSFWActionCreators.agreeToNSFWChannel(channelId);
	};

	const renderContent = () => {
		switch (reason) {
			case NSFWGateReason.GEO_RESTRICTED: {
				const countryCode = GeoIPStore.countryCode;
				const regionCode = GeoIPStore.regionCode;
				const regionName = formatRegion(i18n, countryCode, regionCode);

				return (
					<>
						<h2 className={styles.title}>
							<Trans>Age-Restricted Content</Trans>
						</h2>
						<p className={styles.description}>
							<Trans>
								Due to age verification laws in {regionName}, you cannot access NSFW content from this region.
							</Trans>
						</p>
					</>
				);
			}

			case NSFWGateReason.AGE_RESTRICTED:
				return (
					<>
						<h2 className={styles.title}>
							<Trans>Age-Restricted Channel</Trans>
						</h2>
						<p className={styles.description}>
							<Trans>
								You must be 18 or older to view this channel.{' '}
								<ExternalLink href={HelpCenterUtils.getURL('1426347609450086400')}>Learn more</ExternalLink>
							</Trans>
						</p>
					</>
				);
			default:
				return (
					<>
						<h2 className={styles.title}>
							<Trans>NSFW Channel</Trans>
						</h2>
						<p className={styles.description}>
							<Trans>
								This channel may contain content that is not safe for work or that may be inappropriate for some users.
								You must be 18 or older to view this channel.
							</Trans>
						</p>
						<Button onClick={handleAgree} variant="danger-primary">
							<Trans>I am 18 or older</Trans>
						</Button>
					</>
				);
		}
	};

	return (
		<div className={styles.container}>
			<div className={styles.content}>
				<div className={styles.iconContainer}>
					<WarningIcon className={styles.icon} weight="fill" />
				</div>
				{renderContent()}
			</div>
		</div>
	);
});
