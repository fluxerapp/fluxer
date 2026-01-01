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

import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import * as DateUtils from '~/utils/DateUtils';
import styles from './TimestampWithTooltip.module.css';

interface TimestampWithTooltipProps {
	date: Date;
	children: React.ReactNode;
	className?: string;
}

const renderTimeElement = (date: Date, formattedDateTime: string, content: React.ReactNode) => (
	// biome-ignore lint/a11y/useAriaPropsSupportedByRole: aria-label improves screen reader output for time
	<time dateTime={date.toISOString()} aria-label={formattedDateTime}>
		{content}
	</time>
);

export const TimestampWithTooltip = observer(({date, children, className}: TimestampWithTooltipProps) => {
	const isMobileLayout = MobileLayoutStore.isEnabled();
	const formattedDateTime = DateUtils.getFormattedDateTimeWithSeconds(date);

	const decoratedChildren = (
		<>
			<i className={styles.hiddenSpacer} aria-hidden="true">
				{' — '}
			</i>
			{children}
		</>
	);

	const timeElement = renderTimeElement(date, formattedDateTime, decoratedChildren);

	return (
		<span className={clsx(className, styles.container)}>
			{isMobileLayout ? (
				timeElement
			) : (
				<Tooltip delay={750} text={formattedDateTime} maxWidth="none">
					{timeElement}
				</Tooltip>
			)}
		</span>
	);
});
