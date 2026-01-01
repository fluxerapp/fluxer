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
import {observer} from 'mobx-react-lite';
import {ComparisonCheckRow} from './ComparisonCheckRow';
import {ComparisonRow} from './ComparisonRow';
import styles from './FeatureComparisonTable.module.css';

export const FeatureComparisonTable = observer(({formatter}: {formatter: Intl.NumberFormat}) => {
	const {t} = useLingui();
	return (
		<div className={styles.table}>
			<div className={styles.header}>
				<div className={styles.headerFeature}>
					<p className={styles.headerFeatureText}>
						<Trans>Feature</Trans>
					</p>
				</div>
				<div className={styles.headerValues}>
					<div className={styles.headerFree}>
						<Trans>Free</Trans>
					</div>
					<div className={styles.headerPlutonium}>
						<Trans>Plutonium</Trans>
					</div>
				</div>
			</div>

			<div className={styles.rows}>
				<ComparisonCheckRow feature={t`Custom 4-digit username tag`} freeHas={false} plutoniumHas={true} />
				<ComparisonCheckRow feature={t`Per-community profiles`} freeHas={false} plutoniumHas={true} />
				<ComparisonCheckRow feature={t`Profile badge`} freeHas={false} plutoniumHas={true} />
				<ComparisonRow
					feature={t`Custom video backgrounds`}
					freeValue={formatter.format(1)}
					plutoniumValue={formatter.format(15)}
				/>
				<ComparisonCheckRow feature={t`Custom entrance sounds`} freeHas={false} plutoniumHas={true} />
				<ComparisonCheckRow feature={t`Custom notification sounds`} freeHas={false} plutoniumHas={true} />
				<ComparisonRow
					feature={t`Communities`}
					freeValue={formatter.format(100)}
					plutoniumValue={formatter.format(200)}
				/>
				<ComparisonRow
					feature={t`Message character limit`}
					freeValue={formatter.format(2000)}
					plutoniumValue={formatter.format(4000)}
				/>
				<ComparisonRow
					feature={t`Bookmarked messages`}
					freeValue={formatter.format(50)}
					plutoniumValue={formatter.format(300)}
				/>
				<ComparisonRow
					feature={t`Bio character limit`}
					freeValue={formatter.format(160)}
					plutoniumValue={formatter.format(320)}
				/>
				<ComparisonRow feature={t`File upload size`} freeValue={t`25 MB`} plutoniumValue={t`500 MB`} />
				<ComparisonRow
					feature={t`Saved media`}
					freeValue={formatter.format(50)}
					plutoniumValue={formatter.format(500)}
				/>
				<ComparisonCheckRow feature={t`Use animated emojis`} freeHas={true} plutoniumHas={true} />
				<ComparisonCheckRow feature={t`Global emoji & sticker access`} freeHas={false} plutoniumHas={true} />
				<ComparisonRow feature={t`Video quality`} freeValue={t`720p/30fps`} plutoniumValue={t`Up to 4K/60fps`} />
				<ComparisonCheckRow feature={t`Animated avatars & banners`} freeHas={false} plutoniumHas={true} />
				<ComparisonCheckRow feature={t`Early access to new features`} freeHas={false} plutoniumHas={true} />
			</div>
		</div>
	);
});
