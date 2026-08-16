// SPDX-License-Identifier: AGPL-3.0-or-later

import {DirectSelectionSurface, peekDirectSelection} from '@app/features/app/components/layout/DirectSelectionOrigin';
import {resolveGuildListIndicatorBarTarget} from '@app/features/app/components/layout/sidebar_nav/GuildListIndicator';
import {GuildListItemPresentation} from '@app/features/app/components/layout/sidebar_nav/GuildListItemPresentation';
import type {GuildListItemProps} from '@app/features/app/components/layout/sidebar_nav/GuildListItemProps';
import {GuildListItemTooltip} from '@app/features/app/components/layout/sidebar_nav/GuildListItemTooltip';
import type {GuildListItemDragAndDrop} from '@app/features/app/components/layout/sidebar_nav/UseGuildListItemDragAndDrop';
import {useGuildListItemInteraction} from '@app/features/app/components/layout/sidebar_nav/UseGuildListItemInteraction';
import {useGuildListItemState} from '@app/features/app/components/layout/sidebar_nav/UseGuildListItemState';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import {GuildHeaderBottomSheet} from '@app/features/guild/components/bottomsheets/GuildHeaderBottomSheet';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useRef} from 'react';

interface GuildListItemContentProps extends GuildListItemProps {
	readonly isDesktopLayout: boolean;
	readonly isMobileExperience: boolean;
	readonly itemRef: React.RefObject<HTMLElement | null>;
	readonly dragAndDrop: GuildListItemDragAndDrop | null;
}

function guildIconBorderRadius(isEmphasized: boolean): string {
	if (isEmphasized) {
		return '30%';
	}
	return '50%';
}

export const GuildListItemContent = observer((props: GuildListItemContentProps) => {
	const {i18n} = useLingui();
	const state = useGuildListItemState({
		guild: props.guild,
		guildIndex: props.guildIndex,
		i18n,
		isSelected: props.isSelected,
		selectedGuildIndex: props.selectedGuildIndex,
	});
	const interaction = useGuildListItemInteraction({
		guild: props.guild,
		isDesktopLayout: props.isDesktopLayout,
		isMobileExperience: props.isMobileExperience,
		isSortingList: props.isSortingList,
		itemRef: props.itemRef,
	});
	const focusableRef = useRef<HTMLElement | null>(null);
	const focusRingTargetRef = useRef<HTMLElement | null>(null);
	const didMountRef = useRef(false);
	const dragAndDrop = props.dragAndDrop;
	let dragConnectorRef: ((node: HTMLElement | null) => void) | null = null;
	let dropConnectorRef: ((node: HTMLElement | null) => void) | null = null;
	if (dragAndDrop != null) {
		dragConnectorRef = dragAndDrop.dragConnectorRef;
		dropConnectorRef = dragAndDrop.dropConnectorRef;
	}
	let scrollTargetRef: React.RefCallback<HTMLElement> | null = null;
	if (props.scrollTargetRef != null) {
		scrollTargetRef = props.scrollTargetRef;
	}
	const surfaceRef = useMergeRefs([
		dragConnectorRef,
		dropConnectorRef,
		interaction.hoverRef,
		focusableRef,
		props.itemRef,
		scrollTargetRef,
	]);
	useEffect(() => {
		const isInitialMount = !didMountRef.current;
		didMountRef.current = true;
		const selectedFromThisRow = peekDirectSelection(DirectSelectionSurface.GUILD_RAIL);
		if (isInitialMount || selectedFromThisRow || !props.isSelected) return;
		const focusable = focusableRef.current;
		if (focusable == null) return;
		focusable.scrollIntoView({block: 'nearest'});
	}, [props.isSelected]);
	const showHoverState = interaction.isHovering || interaction.contextMenuOpen;
	const showGuildIndicator = state.hasUnreadMessages || props.isSelected || showHoverState;
	const indicatorTarget = resolveGuildListIndicatorBarTarget({isSelected: props.isSelected, showHoverState});
	const renderTooltip = () => {
		if (props.isSortingList) return null;
		return (
			<GuildListItemTooltip
				guild={props.guild}
				canManageGuild={state.canManageGuild}
				isMuted={state.isMuted}
				mutedText={state.mutedText}
				guildCounts={state.guildCounts}
				currentLocale={state.currentLocale}
				voiceRows={state.voiceRows}
				navigationKeybind={state.navigationKeybind}
				data-flx="app.sidebar-nav.guild-list-item-content.render-tooltip.guild-list-item-tooltip"
			/>
		);
	};
	return (
		<>
			<Tooltip
				position="right"
				maxWidth="xl"
				size="large"
				text={renderTooltip}
				data-flx="app.sidebar-nav.guild-list-item-content.tooltip"
			>
				<FocusRing
					focusTarget={focusableRef}
					ringTarget={focusRingTargetRef}
					offset={-2}
					data-flx="app.sidebar-nav.guild-list-item-content.focus-ring"
				>
					<GuildListItemPresentation
						guild={props.guild}
						isSortingList={props.isSortingList}
						isSelected={props.isSelected}
						contextMenuOpen={interaction.contextMenuOpen}
						showGuildIndicator={showGuildIndicator}
						indicatorTarget={indicatorTarget}
						backgroundImage={interaction.backgroundImage}
						iconBorderRadius={guildIconBorderRadius(props.isSelected || showHoverState)}
						mentionCount={state.mentionCount}
						canManageGuild={state.canManageGuild}
						hasVoiceActivity={state.hasVoiceActivity}
						voiceBadgeActivity={state.voiceBadgeActivity}
						guildARIALabel={state.guildARIALabel}
						focusRingTargetRef={focusRingTargetRef}
						surfaceRef={surfaceRef}
						dragAndDrop={props.dragAndDrop}
						onClick={interaction.handleSelect}
						onContextMenu={interaction.handleContextMenu}
						onKeyDown={interaction.handleKeyDown}
						onLongPress={interaction.handleLongPress}
						data-flx="app.sidebar-nav.guild-list-item-content.guild-list-item-presentation.select"
					/>
				</FocusRing>
			</Tooltip>
			{props.isMobileExperience && (
				<GuildHeaderBottomSheet
					isOpen={interaction.bottomSheetOpen}
					onClose={interaction.handleCloseBottomSheet}
					guild={props.guild}
					data-flx="app.sidebar-nav.guild-list-item-content.guild-header-bottom-sheet"
				/>
			)}
		</>
	);
});
