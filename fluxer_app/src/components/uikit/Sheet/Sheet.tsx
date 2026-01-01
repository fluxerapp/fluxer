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

import {X as XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {Sheet as ModalSheet} from 'react-modal-sheet';
import {useBottomSheetBackHandler} from '~/hooks/useBottomSheetBackHandler';
import OverlayStackStore from '~/stores/OverlayStackStore';
import styles from './Sheet.module.css';

type Surface = 'primary' | 'secondary' | 'tertiary';

interface RootProps {
	isOpen: boolean;
	onClose: () => void;
	children: React.ReactNode;
	initialSnap?: number;
	snapPoints?: Array<number>;
	surface?: Surface;
	showHandle?: boolean;
	zIndex?: number;
	modalEffectRootId?: string;
	backdropOpacity?: number;
	showBackdrop?: boolean;
	className?: string;
}

const surfaceClassMap: Record<Surface, string> = {
	primary: styles.surfacePrimary,
	secondary: styles.surfaceSecondary,
	tertiary: styles.surfaceTertiary,
};

const RootComponent: React.FC<RootProps> = ({
	isOpen,
	onClose,
	children,
	initialSnap = 1,
	snapPoints = [0, 0.6, 1],
	surface = 'secondary',
	zIndex: explicitZIndex,
	modalEffectRootId = 'root',
	backdropOpacity = 0.7,
	showBackdrop = true,
	className,
}) => {
	const [acquiredZIndex, setAcquiredZIndex] = React.useState<number | null>(null);

	React.useEffect(() => {
		if (isOpen) {
			const zIndex = OverlayStackStore.acquire();
			setAcquiredZIndex(zIndex);
			return () => {
				OverlayStackStore.release();
				setAcquiredZIndex(null);
			};
		}
		return undefined;
	}, [isOpen]);

	const zIndex = explicitZIndex ?? acquiredZIndex ?? OverlayStackStore.peek();

	useBottomSheetBackHandler(isOpen, onClose);

	return (
		<ModalSheet
			isOpen={isOpen}
			onClose={onClose}
			snapPoints={snapPoints}
			initialSnap={initialSnap}
			modalEffectRootId={modalEffectRootId}
			style={{zIndex}}
		>
			{showBackdrop && (
				<ModalSheet.Backdrop
					onTap={onClose}
					style={{
						backgroundColor: `rgba(0, 0, 0, ${backdropOpacity})`,
					}}
				/>
			)}
			<ModalSheet.Container className={clsx(styles.container, surfaceClassMap[surface])}>
				<div className={clsx(styles.root, className)}>{children}</div>
			</ModalSheet.Container>
		</ModalSheet>
	);
};

const Root = observer(RootComponent);

interface HandleProps {
	className?: string;
}

const Handle: React.FC<HandleProps> = ({className}) => (
	<ModalSheet.Header className={clsx(styles.handle, className)} disableDrag={false}>
		<div className={styles.handleBar} />
	</ModalSheet.Header>
);

type HeaderAlign = 'center' | 'start' | 'end';

interface HeaderProps {
	children?: React.ReactNode;
	leading?: React.ReactNode;
	trailing?: React.ReactNode;
	border?: boolean;
	align?: HeaderAlign;
	padding?: 'sm' | 'md' | 'lg';
	className?: string;
	safeAreaTop?: boolean;
	after?: React.ReactNode;
}

const headerAlignClassMap: Record<HeaderAlign, string> = {
	center: '',
	start: styles.headerAlignStart,
	end: styles.headerAlignEnd,
};

const headerPaddingClassMap: Record<'sm' | 'md' | 'lg', string> = {
	sm: styles.headerPaddingSm,
	md: styles.headerPaddingMd,
	lg: styles.headerPaddingLg,
};

const Header: React.FC<HeaderProps> = ({
	children,
	leading,
	trailing,
	border = true,
	align = 'center',
	padding = 'md',
	className,
	safeAreaTop = false,
	after,
}) => (
	<div
		className={clsx(
			styles.header,
			border && styles.headerBorder,
			headerPaddingClassMap[padding],
			headerAlignClassMap[align],
			safeAreaTop && styles.headerSafeArea,
			className,
		)}
	>
		<div className={styles.headerGrid}>
			<div className={clsx(styles.headerSlot, styles.headerSlotLeading)}>{leading}</div>
			<div className={styles.headerCenter}>{children}</div>
			<div className={clsx(styles.headerSlot, styles.headerSlotTrailing)}>{trailing}</div>
		</div>
		{after && <div className={styles.headerAfter}>{after}</div>}
	</div>
);

interface TitleProps {
	children: React.ReactNode;
	as?: 'h2' | 'h3' | 'span';
}

const Title: React.FC<TitleProps> = ({children, as: Component = 'h2'}) => (
	<Component className={styles.title}>{children}</Component>
);

interface SubtitleProps {
	children: React.ReactNode;
}

const Subtitle: React.FC<SubtitleProps> = ({children}) => <p className={styles.subtitle}>{children}</p>;

interface ContentProps {
	children: React.ReactNode;
	padding?: 'none' | 'md';
	scrollable?: boolean;
	className?: string;
}

const Content: React.FC<ContentProps> = ({children, padding = 'md', scrollable = true, className}) => (
	<div className={clsx(styles.content, padding === 'none' && styles.contentNoPadding, className)}>
		<div className={clsx(styles.contentInner, !scrollable && styles.contentStatic)}>{children}</div>
	</div>
);

interface SectionProps {
	children: React.ReactNode;
	className?: string;
}

const Section: React.FC<SectionProps> = ({children, className}) => (
	<div className={clsx(styles.section, className)}>{children}</div>
);

interface FooterProps {
	children: React.ReactNode;
	border?: boolean;
	className?: string;
}

const Footer: React.FC<FooterProps> = ({children, border = true, className}) => (
	<div className={clsx(styles.footer, !border && styles.footerNoBorder, className)}>{children}</div>
);

interface ActionsProps {
	children: React.ReactNode;
	className?: string;
}

const Actions: React.FC<ActionsProps> = ({children, className}) => (
	<div className={clsx(styles.actions, className)}>{children}</div>
);

interface DividerProps {
	className?: string;
}

const Divider: React.FC<DividerProps> = ({className}) => <div className={clsx(styles.divider, className)} />;

interface CloseButtonProps {
	onClick: () => void;
	className?: string;
}

const CloseButton: React.FC<CloseButtonProps> = ({onClick, className}) => (
	<button type="button" onClick={onClick} className={clsx(styles.closeButton, className)} aria-label="Close">
		<XIcon weight="bold" />
	</button>
);

export {Root, Handle, Header, Title, Subtitle, Content, Section, Footer, Actions, Divider, CloseButton};
