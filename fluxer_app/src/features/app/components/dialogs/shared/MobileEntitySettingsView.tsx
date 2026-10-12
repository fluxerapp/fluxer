// SPDX-License-Identifier: AGPL-3.0-or-later
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {
	MobileHeader,
	MobileHeaderWithBanner,
	MobileSettingsList,
} from '@app/features/app/components/dialogs/shared/MobileSettingsComponents';
import {SETTINGS_FADE_VARIANTS} from '@app/features/app/components/dialogs/shared/SettingsTransitions';
import {usePreservedScrollerPosition} from '@app/features/app/components/dialogs/shared/UsePreservedScrollerPosition';
import {Scroller} from '@app/features/ui/components/Scroller';
import styles from '@app/features/user/components/modals/UserSettingsModal.module.css';
import type {MobileNavigationState} from '@app/features/user/hooks/useMobileNavigation';
import {useUnsavedChangesFlash} from '@app/features/user/hooks/useUnsavedChangesFlash';
import type {Icon, IconWeight} from '@phosphor-icons/react';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

interface MobileEntityTab {
	type: string;
	label: string;
	icon: Icon;
	iconWeight?: IconWeight;
}
interface MobileEntitySettingsViewProps<T extends MobileEntityTab> {
	groupedSettingsTabs: Record<string, Array<T>>;
	currentTab?: T;
	mobileNav: MobileNavigationState<T['type']>;
	onBack: () => void;
	onTabSelect: (tabType: string, title: string) => void;
	rootTitle: React.ReactNode;
	mobileTitle: React.ReactNode;
	pageLinkHref?: string | null;
	content: React.ReactNode;
	dangerAction: React.ReactNode;
	hiddenCategories: Array<string>;
	categoryLabels?: Record<string, string>;
	contentScrollerClassName: string;
	contentInnerClassName: string;
	contentScrollerKey: string;
}
export const MobileEntitySettingsView = observer(function MobileEntitySettingsView<T extends MobileEntityTab>({
	groupedSettingsTabs,
	currentTab,
	mobileNav,
	onBack,
	onTabSelect,
	rootTitle,
	mobileTitle,
	pageLinkHref,
	content,
	dangerAction,
	hiddenCategories,
	categoryLabels,
	contentScrollerClassName,
	contentInnerClassName,
	contentScrollerKey,
}: MobileEntitySettingsViewProps<T>) {
	const reducedMotion = Accessibility.useReducedMotion;
	const currentTabId = mobileNav.currentView?.tab;
	const {showUnsavedBanner, flashBanner, tabData, checkUnsavedChanges} = useUnsavedChangesFlash(currentTabId);
	const handleBack = useCallback(() => {
		if (checkUnsavedChanges()) return;
		onBack();
	}, [checkUnsavedChanges, onBack]);
	const handleTabSelect = useCallback(
		(tabType: string, title: string) => {
			if (checkUnsavedChanges()) return;
			onTabSelect(tabType, title);
		},
		[checkUnsavedChanges, onTabSelect],
	);
	const showMobileList = mobileNav.isRootView;
	const showMobileContent = !mobileNav.isRootView;
	const {scrollerRef: listScrollerRef, handleScroll: handleListScroll} = usePreservedScrollerPosition(showMobileList);
	return (
		<div className={styles.mobileWrapper} data-flx="app.mobile-entity-settings-view.mobile-wrapper">
			<div className={styles.mobileHeaderContainer} data-flx="app.mobile-entity-settings-view.mobile-header-container">
				<AnimatePresence
					mode="wait"
					custom={mobileNav.direction}
					data-flx="app.mobile-entity-settings-view.animate-presence"
				>
					{showMobileList && (
						<motion.div
							key="mobile-list-header"
							variants={reducedMotion ? undefined : SETTINGS_FADE_VARIANTS}
							initial="center"
							animate="center"
							exit={reducedMotion ? 'center' : 'exit'}
							transition={{duration: reducedMotion ? 0 : 0.08, ease: 'easeInOut'}}
							className={styles.mobileHeaderContent}
							data-flx="app.mobile-entity-settings-view.mobile-header-content"
						>
							<MobileHeader
								title={rootTitle}
								onBack={handleBack}
								data-flx="app.mobile-entity-settings-view.mobile-header"
							/>
						</motion.div>
					)}
					{showMobileContent && currentTab && (
						<motion.div
							key={`mobile-content-header-${mobileNav.currentView?.tab}`}
							variants={reducedMotion ? undefined : SETTINGS_FADE_VARIANTS}
							initial={reducedMotion ? 'center' : 'enter'}
							animate="center"
							exit={reducedMotion ? 'center' : 'exit'}
							transition={{duration: reducedMotion ? 0 : 0.08, ease: 'easeInOut'}}
							className={styles.mobileHeaderContent}
							data-flx="app.mobile-entity-settings-view.mobile-header-content--2"
						>
							<MobileHeaderWithBanner
								pageLinkHref={pageLinkHref}
								title={mobileTitle}
								onBack={handleBack}
								showUnsavedBanner={showUnsavedBanner}
								flashBanner={flashBanner}
								tabData={tabData}
								data-flx="app.mobile-entity-settings-view.mobile-header-with-banner"
							/>
						</motion.div>
					)}
				</AnimatePresence>
			</div>
			<div
				className={styles.mobileContentContainer}
				data-flx="app.mobile-entity-settings-view.mobile-content-container"
			>
				<AnimatePresence
					mode="wait"
					custom={mobileNav.direction}
					data-flx="app.mobile-entity-settings-view.animate-presence--2"
				>
					{showMobileList && (
						<motion.div
							key="mobile-list-content"
							custom={mobileNav.direction}
							variants={reducedMotion ? undefined : SETTINGS_FADE_VARIANTS}
							initial="center"
							animate="center"
							exit={reducedMotion ? 'center' : 'exit'}
							transition={{duration: reducedMotion ? 0 : 0.15, ease: 'easeInOut'}}
							className={styles.mobileContentPane}
							data-flx="app.mobile-entity-settings-view.mobile-content-pane"
						>
							<MobileSettingsList
								groupedTabs={groupedSettingsTabs}
								onTabSelect={handleTabSelect}
								hiddenCategories={hiddenCategories}
								categoryLabels={categoryLabels}
								dangerContent={dangerAction}
								scrollRef={listScrollerRef}
								onScroll={handleListScroll}
								data-flx="app.mobile-entity-settings-view.mobile-settings-list"
							/>
						</motion.div>
					)}
					{showMobileContent && content && (
						<motion.div
							key={`mobile-content-${mobileNav.currentView?.tab}`}
							custom={mobileNav.direction}
							variants={reducedMotion ? undefined : SETTINGS_FADE_VARIANTS}
							initial={reducedMotion ? 'center' : 'enter'}
							animate="center"
							exit={reducedMotion ? 'center' : 'exit'}
							transition={{duration: reducedMotion ? 0 : 0.15, ease: 'easeInOut'}}
							className={styles.mobileContentPane}
							data-flx="app.mobile-entity-settings-view.mobile-content-pane--2"
						>
							<Scroller
								className={contentScrollerClassName}
								key={contentScrollerKey}
								data-flx="app.mobile-entity-settings-view.mobile-content-scroller"
							>
								<div className={contentInnerClassName} data-flx="app.mobile-entity-settings-view.mobile-content-inner">
									{content}
								</div>
							</Scroller>
						</motion.div>
					)}
				</AnimatePresence>
			</div>
		</div>
	);
});
