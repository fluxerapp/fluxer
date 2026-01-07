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
import type React from 'react';
import {Button} from '~/components/uikit/Button/Button';
import styles from './NagbarButton.module.css';

interface NagbarButtonProps {
	children: React.ReactNode;
	onClick: () => void;
	isMobile: boolean;
	className?: string;
	disabled?: boolean;
	submitting?: boolean;
}

export const NagbarButton = ({
	children,
	onClick,
	isMobile,
	className,
	disabled = false,
	submitting,
}: NagbarButtonProps) => {
	return (
		<Button
			variant="inverted-outline"
			superCompact={!isMobile}
			compact={isMobile}
			fitContent
			className={clsx(styles.button, className)}
			onClick={onClick}
			disabled={disabled}
			submitting={submitting}
		>
			{children}
		</Button>
	);
};
