// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/ui/icons/DirectionalIcons.module.css';
import {
	ArrowBendUpLeftIcon as BaseArrowBendUpLeftIcon,
	ArrowBendUpRightIcon as BaseArrowBendUpRightIcon,
	ArrowLeftIcon as BaseArrowLeftIcon,
	ArrowRightIcon as BaseArrowRightIcon,
	CaretLeftIcon as BaseCaretLeftIcon,
	CaretRightIcon as BaseCaretRightIcon,
	type Icon,
	type IconProps,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {forwardRef} from 'react';

function mirrorInRtl(BaseIcon: Icon): Icon {
	const MirroredIcon = forwardRef<SVGSVGElement, IconProps>(({className, ...props}, ref) => (
		<BaseIcon
			ref={ref}
			className={clsx(styles.mirrored, className)}
			data-flx="ui.icons.directional-icons.mirrored-icon.mirrored"
			{...props}
		/>
	));
	MirroredIcon.displayName = BaseIcon.displayName;
	return MirroredIcon;
}

export const ArrowBendUpLeftIcon = mirrorInRtl(BaseArrowBendUpLeftIcon);
export const ArrowBendUpRightIcon = mirrorInRtl(BaseArrowBendUpRightIcon);
export const ArrowLeftIcon = mirrorInRtl(BaseArrowLeftIcon);
export const ArrowRightIcon = mirrorInRtl(BaseArrowRightIcon);
export const CaretLeftIcon = mirrorInRtl(BaseCaretLeftIcon);
export const CaretRightIcon = mirrorInRtl(BaseCaretRightIcon);

export function getCollapsedCaretAngle(): number {
	return document.documentElement.dir === 'rtl' ? 90 : -90;
}
