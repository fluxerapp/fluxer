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

import {Tooltip} from '@app/components/uikit/tooltip/Tooltip';
import type {RendererProps} from '@app/lib/markdown/renderers/RendererTypes';
import {formatTimestamp} from '@app/lib/markdown/utils/DateFormatter';
import {getDateFromUnixTimestampSeconds} from '@app/lib/markdown/utils/TimestampValidation';
import WindowStore from '@app/stores/WindowStore';
import markupStyles from '@app/styles/Markup.module.css';
import timestampRendererStyles from '@app/styles/TimestampRenderer.module.css';
import {getCurrentLocale} from '@app/utils/LocaleUtils';
import {isSameDay} from '@fluxer/date_utils/src/DateComparison';
import {getFormattedDateTimeWithSeconds} from '@fluxer/date_utils/src/DateFormatting';
import {TimestampStyle} from '@fluxer/markdown_parser/src/types/Enums';
import type {TimestampNode} from '@fluxer/markdown_parser/src/types/Nodes';
import {ClockIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {DateTime} from 'luxon';
import {observer} from 'mobx-react-lite';
import React, {type ReactElement, useEffect, useState} from 'react';

export const TimestampRenderer = observer(function TimestampRenderer({
	node,
	id,
	options,
}: RendererProps<TimestampNode>): ReactElement {
	const {timestamp, style} = node;
	const i18n = options.i18n;

	const date = getDateFromUnixTimestampSeconds(timestamp);
	const isValidTimestamp = date !== null;
	const now = new Date();

	const isPast = date !== null && date < now;
	const isFuture = date !== null && date > now;
	const isTodayDate = date !== null && isSameDay(date);

	const locale = getCurrentLocale();
	const fullDateTime = date !== null ? getFormattedDateTimeWithSeconds(date, locale) : null;

	const isRelativeStyle = style === TimestampStyle.RelativeTime;
	const isWindowFocused = WindowStore.focused;
	const [relativeDisplayTime, setRelativeDisplayTime] = useState(() =>
		isValidTimestamp ? formatTimestamp(timestamp, style, i18n) : '',
	);
	const relativeTime = date !== null ? DateTime.fromJSDate(date).toRelative() : null;

	useEffect(() => {
		if (!isValidTimestamp || !isRelativeStyle || !isWindowFocused) {
			return;
		}

		const refreshDisplay = () => {
			setRelativeDisplayTime((previous) => {
				const nextValue = formatTimestamp(timestamp, style, i18n);
				return previous === nextValue ? previous : nextValue;
			});
		};

		refreshDisplay();
		const intervalId = setInterval(refreshDisplay, 1000);
		return () => clearInterval(intervalId);
	}, [isValidTimestamp, isRelativeStyle, isWindowFocused, style, timestamp, i18n]);

	if (date === null || fullDateTime === null) {
		return React.createElement('span', {className: markupStyles.timestamp}, String(timestamp));
	}

	const tooltipContent = (
		<div className={timestampRendererStyles.tooltipContainer}>
			<div className={timestampRendererStyles.tooltipFullDateTime}>{fullDateTime}</div>
			<div className={timestampRendererStyles.tooltipRelativeTime}>{relativeTime}</div>
		</div>
	);

	const displayTime = isRelativeStyle ? relativeDisplayTime : formatTimestamp(timestamp, style, i18n);

	const timestampClasses = clsx(
		markupStyles.timestamp,
		isPast && !isTodayDate && timestampRendererStyles.timestampPast,
		isFuture && timestampRendererStyles.timestampFuture,
		isTodayDate && timestampRendererStyles.timestampToday,
	);

	return (
		<Tooltip key={id} text={() => tooltipContent} position="top" delay={200} maxWidth="xl">
			<time className={timestampClasses} dateTime={date.toISOString()}>
				<ClockIcon className={timestampRendererStyles.clockIcon} />
				{displayTime}
			</time>
		</Tooltip>
	);
});
