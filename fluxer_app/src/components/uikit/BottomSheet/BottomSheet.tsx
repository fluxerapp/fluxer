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
import type React from 'react';
import * as Sheet from '~/components/uikit/Sheet/Sheet';

interface BottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	children: React.ReactNode;
	title?: string;
	initialSnap?: number;
	snapPoints?: Array<number>;
	disablePadding?: boolean;
	disableDefaultHeader?: boolean;
	zIndex?: number;
	showHandle?: boolean;
	showCloseButton?: boolean;
	surface?: 'primary' | 'secondary' | 'tertiary';
	headerSlot?: React.ReactNode;
	leadingAction?: React.ReactNode;
	trailingAction?: React.ReactNode;
	containerClassName?: string;
	contentClassName?: string;
}

export const BottomSheet: React.FC<BottomSheetProps> = observer(
	({
		isOpen,
		onClose,
		children,
		title,
		initialSnap = 1,
		snapPoints = [0, 0.5, 0.8, 1],
		disablePadding = false,
		disableDefaultHeader = false,
		zIndex,
		showHandle = true,
		showCloseButton = true,
		surface = 'secondary',
		headerSlot,
		leadingAction,
		trailingAction,
		containerClassName,
		contentClassName,
	}) => {
		const shouldRenderDefaultHeader =
			!disableDefaultHeader && (!!title || !!leadingAction || !!trailingAction || showCloseButton);

		const renderTrailingContent = () => {
			if (!shouldRenderDefaultHeader) return undefined;
			if (trailingAction && showCloseButton) {
				return (
					<>
						{trailingAction}
						<Sheet.CloseButton onClick={onClose} />
					</>
				);
			}
			if (showCloseButton) {
				return <Sheet.CloseButton onClick={onClose} />;
			}
			return trailingAction;
		};

		return (
			<Sheet.Root
				isOpen={isOpen}
				onClose={onClose}
				snapPoints={snapPoints}
				initialSnap={initialSnap}
				surface={surface}
				zIndex={zIndex}
				className={containerClassName}
			>
				{showHandle && <Sheet.Handle />}
				{shouldRenderDefaultHeader && (
					<Sheet.Header
						leading={leadingAction}
						trailing={renderTrailingContent()}
						safeAreaTop={!showHandle}
						after={headerSlot}
					>
						{title && <Sheet.Title>{title}</Sheet.Title>}
					</Sheet.Header>
				)}
				{!shouldRenderDefaultHeader && headerSlot}
				{disablePadding ? children : <Sheet.Content className={contentClassName}>{children}</Sheet.Content>}
			</Sheet.Root>
		);
	},
);
