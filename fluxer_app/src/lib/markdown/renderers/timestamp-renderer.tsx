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

import {ClockIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {DateTime} from 'luxon';
import {observer} from 'mobx-react-lite';
import type {ReactElement} from 'react';
import {useEffect, useState} from 'react';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import WindowStore from '~/stores/WindowStore';
import markupStyles from '~/styles/Markup.module.css';
import timestampRendererStyles from '~/styles/TimestampRenderer.module.css';
import {TimestampStyle} from '../parser/types/enums';
import type {TimestampNode} from '../parser/types/nodes';
import {formatTimestamp} from '../utils/date-formatter';
import type {RendererProps} from '.';

export const TimestampRenderer = observer(function TimestampRenderer({
	node,
	id,
	options,
}: RendererProps<TimestampNode>): ReactElement {
	const {timestamp, style} = node;
	const i18n = options.i18n;

	const totalMillis = timestamp * 1000;
	const date = DateTime.fromMillis(totalMillis);
	const now = DateTime.now();

	const isPast = date < now;
	const isFuture = date > now;
	const isToday = date.hasSame(now, 'day');

	const tooltipFormat = "EEEE, LLLL d, yyyy 'at' h:mm:ss a ZZZZ";
	const fullDateTime = date.toFormat(tooltipFormat);

	const isRelativeStyle = style === TimestampStyle.RelativeTime;
	const isWindowFocused = WindowStore.focused;
	const [relativeDisplayTime, setRelativeDisplayTime] = useState(() => formatTimestamp(timestamp, style, i18n));
	const relativeTime = date.toRelative();

	useEffect(() => {
		if (!isRelativeStyle || !isWindowFocused) {
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
	}, [isRelativeStyle, isWindowFocused, style, timestamp, i18n]);

	const tooltipContent = (
		<div className={timestampRendererStyles.tooltipContainer}>
			<div className={timestampRendererStyles.tooltipFullDateTime}>{fullDateTime}</div>
			<div className={timestampRendererStyles.tooltipRelativeTime}>{relativeTime}</div>
		</div>
	);

	const displayTime = isRelativeStyle ? relativeDisplayTime : formatTimestamp(timestamp, style, i18n);

	const timestampClasses = clsx(
		markupStyles.timestamp,
		isPast && !isToday && timestampRendererStyles.timestampPast,
		isFuture && timestampRendererStyles.timestampFuture,
		isToday && timestampRendererStyles.timestampToday,
	);

	return (
		<Tooltip key={id} text={() => tooltipContent} position="top" delay={200} maxWidth="xl">
			<time className={timestampClasses} dateTime={date.toISO() ?? ''}>
				<ClockIcon className={timestampRendererStyles.clockIcon} />
				{displayTime}
			</time>
		</Tooltip>
	);
});
