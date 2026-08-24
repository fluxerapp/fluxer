// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {useMediaLoading} from '@app/features/messaging/hooks/useMediaLoading';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type {CSSProperties, FC} from 'react';

const PLACEHOLDER_OVERLAY_STYLE: CSSProperties = {position: 'absolute', inset: 0};

interface PickerThumbnailProps {
	src: string;
	alt: string;
	className?: string;
	thumbHashClassName?: string;
	placeholder?: string | null;
}

export const PickerThumbnail: FC<PickerThumbnailProps> = observer(
	({src, alt, className, thumbHashClassName, placeholder}) => {
		const {loaded, error, thumbHashURL, ref, onLoad, onError} = useMediaLoading(src, placeholder ?? undefined);
		const showThumbHash = (!loaded || error) && Boolean(thumbHashURL);
		return (
			<>
				{!error && (
					<img
						src={src}
						ref={ref}
						alt={alt}
						className={className}
						onLoad={onLoad}
						onError={onError}
						data-flx="channel.pickers.picker-thumbnail.img--2"
					/>
				)}
				<AnimatePresence data-flx="channel.pickers.picker-thumbnail.animate-presence">
					{showThumbHash && (
						<motion.img
							key="thumb-hash"
							src={thumbHashURL}
							alt=""
							aria-hidden
							className={thumbHashClassName ?? className}
							style={PLACEHOLDER_OVERLAY_STYLE}
							initial={{opacity: 1}}
							exit={{opacity: 0}}
							transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2}}
							data-flx="channel.pickers.picker-thumbnail.img"
						/>
					)}
				</AnimatePresence>
			</>
		);
	},
);
