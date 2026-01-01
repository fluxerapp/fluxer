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

import {observer} from 'mobx-react-lite';
import {forwardRef, useMemo} from 'react';
import styles from './ScrollFillerSkeleton.module.css';

interface Props {
	messages: Array<number>;
	attachmentSpecs: Array<[number, {width: number; height: number}] | undefined>;
	groupSpacing: number;
	totalHeight: number;
}

const ScrollFillerSkeleton = observer(
	forwardRef<HTMLDivElement, Props>(function ScrollFillerSkeleton(
		{messages, attachmentSpecs, groupSpacing, totalHeight},
		ref,
	) {
		const seededRandom = useMemo(
			() => (seed: number) => {
				const x = Math.sin(seed) * 10000;
				return x - Math.floor(x);
			},
			[],
		);

		return (
			<div className={styles.wrapper} ref={ref} style={{minHeight: totalHeight}} aria-hidden="true">
				{messages.map((messageCount, groupIndex) => {
					const attachmentSpec = attachmentSpecs[groupIndex];
					const baseSeed = (groupIndex + 1) * 17;
					const usernameWidth = 48 + seededRandom(baseSeed) * 36;
					const timestampWidth = 8 + seededRandom(baseSeed + 3) * 12;

					return (
						<div
							key={groupIndex}
							className={styles.messageGroup}
							style={{
								marginBottom: groupIndex === messages.length - 1 ? 0 : `${groupSpacing}px`,
							}}
						>
							<div className={styles.group}>
								<div className={styles.avatar} />
								<div className={styles.body}>
									<div className={styles.header}>
										<div className={styles.username} style={{width: `${Math.min(usernameWidth, 92)}%`}} />
										<div className={styles.timestamp} style={{width: `${Math.min(timestampWidth, 24)}%`}} />
									</div>

									<div className={styles.messages}>
										{Array.from({length: messageCount}).map((_, lineIndex) => {
											const lineSeed = baseSeed + lineIndex * 11;
											const baseWidth = 75;
											const variance = 18;
											const width = baseWidth + seededRandom(lineSeed) * variance;
											return (
												<div
													key={lineIndex}
													className={styles.messageLine}
													style={{
														width: `${Math.min(98, width)}%`,
														height: 12,
													}}
												/>
											);
										})}
									</div>

									{attachmentSpec && (
										<div
											className={styles.attachment}
											style={{
												width: Math.min(attachmentSpec[1].width, 420),
												height: Math.min(attachmentSpec[1].height, 250),
											}}
										/>
									)}
								</div>
							</div>
						</div>
					);
				})}
			</div>
		);
	}),
);

export default ScrollFillerSkeleton;
