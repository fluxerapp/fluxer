// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/ScrollFillerSkeleton.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {observer} from 'mobx-react-lite';
import {forwardRef, useMemo} from 'react';

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
			<div
				className={styles.wrapper}
				ref={ref}
				style={{minHeight: remFromPx(totalHeight)}}
				aria-hidden="true"
				data-flx="channel.scroll-filler-skeleton.wrapper"
			>
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
								marginBottom: groupIndex === messages.length - 1 ? 0 : remFromPx(groupSpacing),
							}}
							data-flx="channel.scroll-filler-skeleton.message-group"
						>
							<div className={styles.group} data-flx="channel.scroll-filler-skeleton.group">
								<div className={styles.avatar} data-flx="channel.scroll-filler-skeleton.avatar" />
								<div className={styles.body} data-flx="channel.scroll-filler-skeleton.body">
									<div className={styles.header} data-flx="channel.scroll-filler-skeleton.header">
										<div
											className={styles.username}
											style={{width: `${Math.min(usernameWidth, 92)}%`}}
											data-flx="channel.scroll-filler-skeleton.username"
										/>
										<div
											className={styles.timestamp}
											style={{width: `${Math.min(timestampWidth, 24)}%`}}
											data-flx="channel.scroll-filler-skeleton.timestamp"
										/>
									</div>
									<div className={styles.messages} data-flx="channel.scroll-filler-skeleton.messages">
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
														height: remFromPx(12),
													}}
													data-flx="channel.scroll-filler-skeleton.message-line"
												/>
											);
										})}
									</div>
									{attachmentSpec && (
										<div
											className={styles.attachment}
											style={{
												width: remFromPx(Math.min(attachmentSpec[1].width, 420)),
												height: remFromPx(Math.min(attachmentSpec[1].height, 250)),
											}}
											data-flx="channel.scroll-filler-skeleton.attachment"
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
