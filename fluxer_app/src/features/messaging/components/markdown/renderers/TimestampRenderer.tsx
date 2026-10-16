// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RendererProps} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {formatTimestamp} from '@app/features/messaging/utils/markdown/DateFormatter';
import {TimestampStyle} from '@app/features/messaging/utils/markdown/parser/Enums';
import type {TimestampNode} from '@app/features/messaging/utils/markdown/parser/Nodes';
import {getDateFromUnixTimestampSeconds} from '@app/features/messaging/utils/markdown/TimestampValidation';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import timestampRendererStyles from '@app/features/theme/styles/TimestampRenderer.module.css';
import Tick from '@app/features/ui/state/Tick';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {getFormattedDateTimeWithSeconds} from '@fluxer/date_utils/src/DateFormatting';
import {DateTime} from 'luxon';
import {observer} from 'mobx-react-lite';
import React, {type ReactElement, useMemo} from 'react';

export const TimestampRenderer = observer(function TimestampRenderer({
	node,
	id,
	options,
}: RendererProps<TimestampNode>): ReactElement {
	const {timestamp, style} = node;
	const i18n = options.i18n;
	const date = getDateFromUnixTimestampSeconds(timestamp);
	const isValidTimestamp = date !== null;
	const locale = getCurrentLocale();
	const fullDateTime = date !== null ? getFormattedDateTimeWithSeconds(date, locale) : null;
	const isRelativeStyle = style === TimestampStyle.RelativeTime;
	const tick = isRelativeStyle ? Tick.nowSecond : 0;
	const relativeDisplayTime = useMemo(() => {
		return isValidTimestamp ? formatTimestamp(timestamp, style, i18n) : '';
	}, [tick, isValidTimestamp, timestamp, style, i18n.locale]);
	const relativeTime = date !== null ? DateTime.fromJSDate(date).setLocale(locale).toRelative() : null;
	if (date === null || fullDateTime === null) {
		return React.createElement('span', {className: markupStyles.timestamp}, String(timestamp));
	}
	const tooltipContent = (
		<div
			className={timestampRendererStyles.tooltipContainer}
			data-flx="messaging.markdown.renderers.timestamp-renderer.div"
		>
			<div
				className={timestampRendererStyles.tooltipFullDateTime}
				data-flx="messaging.markdown.renderers.timestamp-renderer.div--2"
			>
				{fullDateTime}
			</div>
			<div
				className={timestampRendererStyles.tooltipRelativeTime}
				data-flx="messaging.markdown.renderers.timestamp-renderer.div--3"
			>
				{relativeTime}
			</div>
		</div>
	);
	const displayTime = isRelativeStyle ? relativeDisplayTime : formatTimestamp(timestamp, style, i18n);
	return (
		<Tooltip
			key={id}
			text={() => tooltipContent}
			position="top"
			delay={200}
			maxWidth="xl"
			data-flx="messaging.markdown.renderers.timestamp-renderer.tooltip"
		>
			<time
				className={markupStyles.timestamp}
				dateTime={date.toISOString()}
				data-flx="messaging.markdown.renderers.timestamp-renderer.time"
			>
				{displayTime}
			</time>
		</Tooltip>
	);
});
