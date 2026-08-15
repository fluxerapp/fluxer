// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ConnectionIssuesLinks} from '@app/features/app/components/ConnectionIssuesLinks';
import {NativeDragRegion} from '@app/features/app/components/layout/NativeDragRegion';
import styles from '@app/features/app/components/layout/SplashScreen.module.css';
import {useSplashScreenGuard} from '@app/features/app/hooks/useSplashScreenGuard';
import Initialization from '@app/features/app/state/Initialization';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import {FluxerIcon} from '@app/features/ui/components/icons/FluxerIcon';
import {getReducedMotionProps} from '@app/features/ui/utils/ReducedMotionAnimation';
import StatusPage from '@app/features/user/state/StatusPage';
import {type SplashQuote, useSplashQuotes} from '@app/media/data/SplashQuotes';
import {ExternalUrls} from '@fluxer/constants/src/ExternalUrls';
import {Trans} from '@lingui/react/macro';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef, useState} from 'react';

const PROBLEMS_DELAY = 10000;
const STATUS_PAGE_DISPLAY_DELAY = 5000;
const FORCE_DISMISS_TIMEOUT_MS = 3000;

type SplashScreenMode = 'live' | 'outage';

export const SplashScreen = observer(() => {
	const shouldBypass = DeveloperOptions.bypassSplashScreen;
	const interrupted = GatewayConnection.isConnectionInterrupted;
	const isInitialized = Initialization.canNavigateToProtectedRoutes;
	const isReady = !interrupted && isInitialized;
	const [hasTimedOut, setHasTimedOut] = useState(false);

	useEffect(() => {
		if (!isReady) {
			setHasTimedOut(false);
			return;
		}
		const timer = setTimeout(() => setHasTimedOut(true), FORCE_DISMISS_TIMEOUT_MS);
		return () => clearTimeout(timer);
	}, [isReady]);

	if (shouldBypass || hasTimedOut) return null;
	return (
		<AnimatePresence initial={false} data-flx="app.splash-screen.animate-presence">
			{!isReady && <SplashScreenContent mode="live" data-flx="app.splash-screen.splash-screen-content" />}
		</AnimatePresence>
	);
});
export const OutageSplashScreen = observer(() => (
	<SplashScreenContent mode="outage" data-flx="app.splash-screen.outage-splash-screen.splash-screen-content" />
));
const SPLASH_MOTION = {
	initial: {opacity: 0},
	animate: {opacity: 1},
	exit: {opacity: 0},
	transition: {duration: 0.35},
};
const CONNECTION_ISSUES_MOTION = {
	initial: {opacity: 0},
	animate: {opacity: 1},
	exit: {opacity: 0},
	transition: {duration: 0.4},
};
const INCIDENT_CTA_MOTION = {
	initial: {opacity: 0},
	animate: {opacity: 1},
	exit: {opacity: 0},
	transition: {duration: 0.2},
};

interface SplashScreenContentProps {
	mode: SplashScreenMode;
}

function pickRandomQuote(quotes: ReadonlyArray<SplashQuote>): SplashQuote {
	return quotes[Math.floor(Math.random() * quotes.length)];
}

const SplashScreenContent = observer(({mode}: SplashScreenContentProps) => {
	const splashOverlayRef = useRef<HTMLDivElement | null>(null);
	useSplashScreenGuard(splashOverlayRef);
	const isOutageMode = mode === 'outage';
	const splashMotion = getReducedMotionProps(SPLASH_MOTION, Accessibility.useReducedMotion);
	const connectionIssuesMotion = getReducedMotionProps(CONNECTION_ISSUES_MOTION, Accessibility.useReducedMotion);
	const incidentCtaMotion = getReducedMotionProps(INCIDENT_CTA_MOTION, Accessibility.useReducedMotion);
	const interrupted = isOutageMode ? true : GatewayConnection.isConnectionInterrupted;
	const isInitialized = isOutageMode ? false : Initialization.canNavigateToProtectedRoutes;
	const isReady = !interrupted && isInitialized;
	const splashQuotes = useSplashQuotes();
	const incident = StatusPage.incident;
	const quoteRef = useRef<SplashQuote | null>(null);
	if (quoteRef.current == null) {
		quoteRef.current = pickRandomQuote(splashQuotes);
	}
	const quote = quoteRef.current;
	const [showStatusData, setShowStatusData] = useState(isOutageMode);
	const [showProblems, setShowProblems] = useState(isOutageMode);
	const problemsTimerRef = useRef<NodeJS.Timeout | null>(null);
	const clearProblemsTimer = useCallback(() => {
		if (problemsTimerRef.current != null) {
			clearTimeout(problemsTimerRef.current);
			problemsTimerRef.current = null;
		}
	}, []);
	useEffect(() => {
		if (isOutageMode) {
			setShowStatusData(true);
			return;
		}
		const timer = setTimeout(() => setShowStatusData(true), STATUS_PAGE_DISPLAY_DELAY);
		return () => clearTimeout(timer);
	}, [isOutageMode]);
	useEffect(() => {
		if (isOutageMode) {
			clearProblemsTimer();
			setShowProblems(true);
			void StatusPage.checkIncidents();
			return;
		}
		if (isReady) {
			clearProblemsTimer();
			setShowProblems(false);
			return;
		}
		problemsTimerRef.current = setTimeout(() => {
			setShowProblems(true);
			void StatusPage.checkIncidents();
		}, PROBLEMS_DELAY);
		return clearProblemsTimer;
	}, [clearProblemsTimer, isOutageMode, isReady]);
	const liveIncident = showStatusData ? incident : null;
	const frozenDisplayRef = useRef<{incident: typeof liveIncident; text: string} | null>(null);
	if (!isReady) {
		frozenDisplayRef.current = {
			incident: liveIncident,
			text: liveIncident ? liveIncident.name : quote.text,
		};
	}
	const visibleIncident = frozenDisplayRef.current?.incident ?? liveIncident;
	const displayText = frozenDisplayRef.current?.text ?? quote.text;
	const incidentUrl = visibleIncident?.url ?? ExternalUrls.SERVICE_STATUS;
	if (isOutageMode) {
		return (
			<div
				ref={splashOverlayRef}
				className={styles.splashOverlay}
				data-flx="app.splash-screen.splash-screen-content.splash-overlay"
			>
				<NativeDragRegion
					className={styles.topDragRegion}
					data-flx="app.splash-screen.splash-screen-content.top-drag-region"
				/>
				<div className={styles.splashContent} data-flx="app.splash-screen.splash-screen-content.splash-content">
					<div className={styles.iconWrapper} data-flx="app.splash-screen.splash-screen-content.icon-wrapper">
						<div
							className={`${styles.iconPulse} ${styles.iconPulseStatic}`}
							data-flx="app.splash-screen.splash-screen-content.icon-pulse"
						/>
						<FluxerIcon className={styles.icon} data-flx="app.splash-screen.splash-screen-content.icon" />
					</div>
					<div className={styles.quoteContainer} data-flx="app.splash-screen.splash-screen-content.quote-container">
						{visibleIncident != null ? (
							<a
								href={incidentUrl}
								target="_blank"
								rel="noopener noreferrer"
								className={styles.quoteLink}
								data-flx="app.splash-screen.splash-screen-content.quote-link"
							>
								{displayText}
							</a>
						) : (
							<p className={styles.quote} data-flx="app.splash-screen.splash-screen-content.quote">
								<Trans>Connection lost</Trans>
							</p>
						)}
						{visibleIncident != null && (
							<a
								href={incidentUrl}
								target="_blank"
								rel="noopener noreferrer"
								className={styles.incidentCta}
								data-flx="app.splash-screen.splash-screen-content.incident-cta"
							>
								<Trans>View on status page</Trans>
							</a>
						)}
					</div>
				</div>
				{showProblems && !isReady && visibleIncident == null && (
					<div
						className={styles.connectionIssuesOverlay}
						data-flx="app.splash-screen.splash-screen-content.connection-issues-overlay"
					>
						<ConnectionIssuesLinks
							incident={incident}
							data-flx="app.splash-screen.splash-screen-content.connection-issues-links"
						/>
					</div>
				)}
			</div>
		);
	}
	return (
		<motion.div
			ref={splashOverlayRef}
			data-flx="app.splash-screen.splash-screen-content.splash-overlay--2"
			{...splashMotion}
			className={styles.splashOverlay}
			style={{pointerEvents: isReady ? 'none' : 'auto'}}
		>
			<NativeDragRegion
				className={styles.topDragRegion}
				data-flx="app.splash-screen.splash-screen-content.top-drag-region--2"
			/>
			<div className={styles.splashContent} data-flx="app.splash-screen.splash-screen-content.splash-content--2">
				<div className={styles.iconWrapper} data-flx="app.splash-screen.splash-screen-content.icon-wrapper--2">
					<div className={styles.iconPulse} data-flx="app.splash-screen.splash-screen-content.icon-pulse--2" />
					<FluxerIcon className={styles.icon} data-flx="app.splash-screen.splash-screen-content.icon--2" />
				</div>
				<div className={styles.quoteContainer} data-flx="app.splash-screen.splash-screen-content.quote-container--2">
					{visibleIncident != null ? (
						<a
							href={incidentUrl}
							target="_blank"
							rel="noopener noreferrer"
							className={styles.quoteLink}
							data-flx="app.splash-screen.splash-screen-content.quote-link--2"
						>
							{displayText}
						</a>
					) : (
						<>
							<p className={styles.quote} data-flx="app.splash-screen.splash-screen-content.quote--2">
								{displayText}
							</p>
							<p className={styles.quoteSource} data-flx="app.splash-screen.splash-screen-content.quote-source">
								{quote.source}
							</p>
						</>
					)}
					<AnimatePresence data-flx="app.splash-screen.splash-screen-content.animate-presence">
						{visibleIncident != null && (
							<motion.a
								data-flx="app.splash-screen.splash-screen-content.incident-cta--2"
								{...incidentCtaMotion}
								href={incidentUrl}
								target="_blank"
								rel="noopener noreferrer"
								className={styles.incidentCta}
							>
								<Trans>View on status page</Trans>
							</motion.a>
						)}
					</AnimatePresence>
				</div>
			</div>
			<AnimatePresence data-flx="app.splash-screen.splash-screen-content.animate-presence--2">
				{showProblems && !isReady && visibleIncident == null && (
					<motion.div
						data-flx="app.splash-screen.splash-screen-content.connection-issues-overlay--2"
						{...connectionIssuesMotion}
						className={styles.connectionIssuesOverlay}
					>
						<ConnectionIssuesLinks
							incident={incident}
							data-flx="app.splash-screen.splash-screen-content.connection-issues-links--2"
						/>
					</motion.div>
				)}
			</AnimatePresence>
		</motion.div>
	);
});
