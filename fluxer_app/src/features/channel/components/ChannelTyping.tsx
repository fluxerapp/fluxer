// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/theme/styles/Typing.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type {CSSProperties} from 'react';
import {useMemo} from 'react';

const TYPING_INDICATOR_DESCRIPTOR = msg({
	message: 'Typing indicator',
	comment: 'Short label in the channel and chat typing. Keep it concise.',
});

interface TypingProps {
	className?: string;
	size?: number;
	style?: CSSProperties;
	color?: string;
}

export const Typing = observer(
	({className, size = 40, style, color = 'var(--typing-indicator-color, var(--text-chat))'}: TypingProps) => {
		const {i18n} = useLingui();
		const scale = useMemo(() => size / 40, [size]);
		const x = useMemo(() => 3.75 * scale, [scale]);
		const y = useMemo(() => 7.5 * scale, [scale]);
		const width = useMemo(() => 17.5 * scale, [scale]);
		const height = useMemo(() => 5 * scale, [scale]);
		const viewBoxWidth = 20;
		const viewBoxHeight = 5;
		const mergedStyle = useMemo(() => ({...(style || {}), color}), [style, color]);
		return (
			<svg
				x={x}
				y={y}
				width={remFromPx(width)}
				height={remFromPx(height)}
				viewBox={`0 0 ${viewBoxWidth} ${viewBoxHeight}`}
				className={className}
				style={mergedStyle}
				role="img"
				aria-label={i18n._(TYPING_INDICATOR_DESCRIPTOR)}
				data-flx="channel.typing.img"
			>
				<circle cx="2.5" cy="2.5" r="2.5" className={styles.dot} fill={color} data-flx="channel.typing.dot" />
				<circle cx="8.75" cy="2.5" r="2.5" className={styles.dot} fill={color} data-flx="channel.typing.dot--2" />
				<circle cx={15} cy="2.5" r="2.5" className={styles.dot} fill={color} data-flx="channel.typing.dot--3" />
			</svg>
		);
	},
);
