// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/GifPicker.module.css';
import {buildGifPickerLoadingSkeletonLayout} from '@app/features/channel/components/pickers/gif/GifPickerLoadingSkeletonGridLayout';
import {motion, useReducedMotion} from 'framer-motion';
import {useLayoutEffect, useMemo, useRef, useState} from 'react';

interface SkeletonViewportSize {
	width: number;
	height: number;
}

const EMPTY_SIZE: SkeletonViewportSize = {width: 0, height: 0};

export function GifPickerLoadingSkeletonGrid() {
	const containerRef = useRef<HTMLDivElement>(null);
	const [viewportSize, setViewportSize] = useState<SkeletonViewportSize>(EMPTY_SIZE);
	const prefersReducedMotion = useReducedMotion();
	useLayoutEffect(() => {
		const element = containerRef.current;
		if (!element) {
			return;
		}
		const updateViewportSize = () => {
			const rect = element.getBoundingClientRect();
			setViewportSize((current) => {
				if (current.width === rect.width && current.height === rect.height) {
					return current;
				}
				return {width: rect.width, height: rect.height};
			});
		};
		updateViewportSize();
		if (typeof ResizeObserver === 'undefined') {
			return;
		}
		const resizeObserver = new ResizeObserver(updateViewportSize);
		resizeObserver.observe(element);
		return () => resizeObserver.disconnect();
	}, []);
	const layout = useMemo(
		() =>
			buildGifPickerLoadingSkeletonLayout({
				viewportWidth: viewportSize.width,
				viewportHeight: viewportSize.height,
			}),
		[viewportSize.height, viewportSize.width],
	);
	return (
		<div
			ref={containerRef}
			className={styles.loadingSkeletonGrid}
			aria-hidden={true}
			data-flx="channel.pickers.gif.gif-picker-loading-skeleton-grid"
		>
			{layout.map((item) => (
				<motion.div
					key={item.key}
					className={styles.loadingSkeletonBlob}
					initial={prefersReducedMotion ? {opacity: 1, y: 0} : {opacity: 0, y: 14}}
					animate={{opacity: 1, y: 0}}
					transition={
						prefersReducedMotion
							? {duration: 0}
							: {
									duration: 0.42,
									delay: item.delaySeconds,
									ease: [0.22, 1, 0.36, 1],
								}
					}
					style={{
						left: item.left,
						top: item.top,
						width: item.renderedWidth,
						height: item.renderedHeight,
					}}
					data-flx="channel.pickers.gif.gif-picker-loading-skeleton-grid.loading-skeleton-blob"
				/>
			))}
		</div>
	);
}
