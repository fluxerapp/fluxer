// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {makeAutoObservable, reaction, runInAction} from 'mobx';

type IncidentStatus = 'investigating' | 'identified' | 'monitoring' | 'resolved';
type IncidentImpact = 'critical' | 'major' | 'minor' | 'maintenance';
export type MaintenanceStatus = 'scheduled' | 'in_progress' | 'completed';

interface StatusPageIncident {
	id: string;
	name: string;
	status: IncidentStatus;
	impact: IncidentImpact;
	url: string;
	updatedAt: string;
}

export interface StatusPageMaintenance {
	id: string;
	name: string;
	status: MaintenanceStatus;
	start: string;
	durationMinutes: number;
	url: string;
}

const INCIDENT_STATUS_MAP: Record<string, IncidentStatus> = {
	INVESTIGATING: 'investigating',
	IDENTIFIED: 'identified',
	MONITORING: 'monitoring',
	RESOLVED: 'resolved',
};
const INCIDENT_IMPACT_MAP: Record<string, IncidentImpact> = {
	MAJOROUTAGE: 'critical',
	PARTIALOUTAGE: 'major',
	DEGRADEDPERFORMANCE: 'minor',
	UNDERMAINTENANCE: 'maintenance',
};
const MAINTENANCE_STATUS_MAP: Record<string, MaintenanceStatus> = {
	NOTSTARTEDYET: 'scheduled',
	INPROGRESS: 'in_progress',
	COMPLETED: 'completed',
};

interface InstatusIncident {
	id: string;
	name: string;
	status: string;
	impact: string;
	url: string;
	updated?: string;
	updatedAt?: string;
	resolved?: string | null;
}

interface InstatusMaintenance {
	id: string;
	name: string;
	status: string;
	url: string;
	start: string;
	duration: string | number;
	updatedAt?: string;
}

interface InstatusPage {
	status?: string;
}

interface InstatusSummary {
	page?: InstatusPage;
	activeIncidents?: Array<InstatusIncident>;
	activeMaintenances?: Array<InstatusMaintenance>;
}

interface InstatusComponent {
	activeMaintenances?: Array<InstatusMaintenance>;
}

type InstatusComponentsResponse = Array<InstatusComponent> | {components?: Array<InstatusComponent>};

const logger = new Logger('StatusPage');
const STATUS_PAGE_POLL_INTERVAL_MS = 60 * 1000;
const STATUS_PAGE_POLL_JITTER_MS = 15 * 1000;
const POLL_ACTIVE_MAINTENANCE_INTERVAL_MS = 60 * 1000;
const POLL_ACTIVE_MAINTENANCE_JITTER_MS = 15 * 1000;
const POLL_AFTER_MAINTENANCE_START_MS = 5 * 1000;
const POLL_MIN_DELAY_MS = 10 * 1000;
const POLL_RESUME_STALE_MS = 30 * 1000;
const STATUS_PAGE_FETCH_TIMEOUT_MS = 10 * 1000;
const STATUS_PAGE_RETRY_DELAYS_MS = [60 * 1000, 5 * 60 * 1000, 15 * 60 * 1000, 30 * 60 * 1000];

function statusPageFetchOptions(): RequestInit {
	return {cache: 'no-store', signal: AbortSignal.timeout(STATUS_PAGE_FETCH_TIMEOUT_MS)};
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function isInstatusSummary(data: unknown): data is InstatusSummary {
	return (
		isRecord(data) &&
		(isRecord(data.page) || Array.isArray(data.activeIncidents) || Array.isArray(data.activeMaintenances))
	);
}

function isUnsupportedStatusPageResponse(response: Response): boolean {
	return response.status >= 400 && response.status < 500 && response.status !== 408 && response.status !== 429;
}

function computeRetryDelay(consecutiveFailures: number): number {
	const index = Math.min(consecutiveFailures, STATUS_PAGE_RETRY_DELAYS_MS.length) - 1;
	return STATUS_PAGE_RETRY_DELAYS_MS[index];
}

function getIncidentStatusPageUrl(): string {
	return RuntimeConfig.statusPageInstatus ? RuntimeConfig.statusPageUrl : '';
}

function computePollDelay(scheduledMaintenance: StatusPageMaintenance | null): number {
	if (scheduledMaintenance?.status === 'in_progress') {
		return POLL_ACTIVE_MAINTENANCE_INTERVAL_MS + Math.random() * POLL_ACTIVE_MAINTENANCE_JITTER_MS;
	}
	if (scheduledMaintenance?.status === 'scheduled') {
		const startTime = new Date(scheduledMaintenance.start).getTime();
		if (Number.isFinite(startTime)) {
			const delayUntilStart = startTime - Date.now();
			if (delayUntilStart <= 0) {
				return POLL_MIN_DELAY_MS;
			}
			if (delayUntilStart < STATUS_PAGE_POLL_INTERVAL_MS) {
				return Math.max(POLL_MIN_DELAY_MS, delayUntilStart + POLL_AFTER_MAINTENANCE_START_MS);
			}
		}
	}
	return STATUS_PAGE_POLL_INTERVAL_MS + Math.random() * STATUS_PAGE_POLL_JITTER_MS;
}

function parseMaintenanceDurationMinutes(duration: string | number | null | undefined): number | null {
	if (duration == null) {
		return null;
	}
	const parsedDuration = Number.parseInt(String(duration), 10);
	if (!Number.isFinite(parsedDuration) || parsedDuration <= 0) {
		return null;
	}
	return parsedDuration;
}

function normalizeMaintenanceStatus(status: string): MaintenanceStatus | null {
	return MAINTENANCE_STATUS_MAP[status] ?? null;
}

function normalizeMaintenance(maintenance: InstatusMaintenance): StatusPageMaintenance | null {
	const status = normalizeMaintenanceStatus(maintenance.status);
	const durationMinutes = parseMaintenanceDurationMinutes(maintenance.duration);
	if (!status || !maintenance.start || durationMinutes == null) {
		return null;
	}
	return {
		id: maintenance.id,
		name: maintenance.name,
		status,
		start: maintenance.start,
		durationMinutes,
		url: maintenance.url,
	};
}

function getMaintenanceStatusPriority(status: MaintenanceStatus): number {
	switch (status) {
		case 'in_progress':
			return 0;
		case 'scheduled':
			return 1;
		case 'completed':
			return 2;
	}
}

function selectActiveStatusPageMaintenance(
	maintenances: ReadonlyArray<InstatusMaintenance> | null | undefined,
): StatusPageMaintenance | null {
	let selectedMaintenance: StatusPageMaintenance | null = null;
	for (const maintenance of maintenances ?? []) {
		const normalizedMaintenance = normalizeMaintenance(maintenance);
		if (!normalizedMaintenance || normalizedMaintenance.status === 'completed') {
			continue;
		}
		if (
			!selectedMaintenance ||
			getMaintenanceStatusPriority(normalizedMaintenance.status) <
				getMaintenanceStatusPriority(selectedMaintenance.status)
		) {
			selectedMaintenance = normalizedMaintenance;
		}
	}
	return selectedMaintenance;
}

function shouldFetchComponentMaintenances(
	data: InstatusSummary,
	activeMaintenance: StatusPageMaintenance | null,
): boolean {
	return !activeMaintenance && data.page?.status === 'UNDERMAINTENANCE';
}

function extractComponentMaintenances(data: InstatusComponentsResponse): Array<InstatusMaintenance> {
	const components = Array.isArray(data) ? data : (data.components ?? []);
	return components.flatMap((component) => component.activeMaintenances ?? []);
}

class StatusPage {
	incident: StatusPageIncident | null = null;
	scheduledMaintenance: StatusPageMaintenance | null = null;
	pollTimerId: NodeJS.Timeout | null = null;
	private checkInFlight: Promise<void> | null = null;
	private checkInFlightUrl: string | null = null;
	private lastCheckedAt = 0;
	private pollingRequested = false;
	private pollingStarted = false;
	private unsupportedStatusPageUrl: string | null = null;
	private consecutiveFailures = 0;

	constructor() {
		makeAutoObservable<
			StatusPage,
			| 'checkInFlight'
			| 'checkInFlightUrl'
			| 'lastCheckedAt'
			| 'pollingRequested'
			| 'pollingStarted'
			| 'pollTimerId'
			| 'unsupportedStatusPageUrl'
			| 'consecutiveFailures'
		>(
			this,
			{
				checkInFlight: false,
				checkInFlightUrl: false,
				lastCheckedAt: false,
				pollingRequested: false,
				pollingStarted: false,
				pollTimerId: false,
				unsupportedStatusPageUrl: false,
				consecutiveFailures: false,
			},
			{autoBind: true},
		);
		reaction(
			() => getIncidentStatusPageUrl(),
			() => this.handleStatusPageUrlChange(),
		);
	}

	private handleStatusPageUrlChange(): void {
		this.incident = null;
		this.scheduledMaintenance = null;
		this.checkInFlight = null;
		this.checkInFlightUrl = null;
		this.lastCheckedAt = 0;
		this.unsupportedStatusPageUrl = null;
		this.consecutiveFailures = 0;
		this.stopActivePolling();
		this.reconcilePolling();
	}

	startPolling(): void {
		this.pollingRequested = true;
		this.reconcilePolling();
	}

	stopPolling(): void {
		this.pollingRequested = false;
		this.stopActivePolling();
	}

	private reconcilePolling(): void {
		const statusPageUrl = getIncidentStatusPageUrl();
		if (!this.pollingRequested || !statusPageUrl || statusPageUrl === this.unsupportedStatusPageUrl) {
			this.stopActivePolling();
			return;
		}
		if (this.pollingStarted) {
			return;
		}
		this.pollingStarted = true;
		this.addResumeListeners();
		void this.checkIncidents();
		this.schedulePoll();
	}

	private stopActivePolling(): void {
		if (!this.pollingStarted) {
			return;
		}
		this.pollingStarted = false;
		this.removeResumeListeners();
		this.clearPollTimer();
	}

	private schedulePoll(): void {
		if (!this.pollingStarted) {
			return;
		}
		this.clearPollTimer();
		this.pollTimerId = setTimeout(() => {
			this.pollTimerId = null;
			if (document.visibilityState === 'visible') {
				void this.refreshAndReschedule();
			} else {
				this.schedulePoll();
			}
		}, this.computeNextDelay());
	}

	private computeNextDelay(): number {
		if (this.consecutiveFailures > 0) {
			return computeRetryDelay(this.consecutiveFailures);
		}
		return computePollDelay(this.scheduledMaintenance);
	}

	async checkIncidents(): Promise<void> {
		const statusPageUrl = getIncidentStatusPageUrl();
		if (!statusPageUrl || statusPageUrl === this.unsupportedStatusPageUrl) {
			return;
		}
		if (this.checkInFlight !== null && this.checkInFlightUrl === statusPageUrl) {
			return this.checkInFlight;
		}
		const pending = this.fetchIncidents(statusPageUrl);
		this.checkInFlight = pending;
		this.checkInFlightUrl = statusPageUrl;
		try {
			await pending;
		} finally {
			if (this.checkInFlight === pending) {
				this.lastCheckedAt = Date.now();
				this.checkInFlight = null;
				this.checkInFlightUrl = null;
			}
		}
	}

	private async fetchIncidents(statusPageUrl: string): Promise<void> {
		try {
			const response = await fetch(`${statusPageUrl}/summary.json`, statusPageFetchOptions());
			if (!response.ok) {
				if (isUnsupportedStatusPageResponse(response)) {
					this.markStatusPageUnsupported(statusPageUrl);
				} else {
					this.recordFailure(statusPageUrl);
				}
				return;
			}
			let data: unknown;
			try {
				data = await response.json();
			} catch (error) {
				if (!(error instanceof SyntaxError)) {
					throw error;
				}
				this.markStatusPageUnsupported(statusPageUrl);
				return;
			}
			if (!isInstatusSummary(data)) {
				this.markStatusPageUnsupported(statusPageUrl);
				return;
			}
			const activeIncident = data.activeIncidents?.find((inc) => inc.status !== 'RESOLVED' && inc.resolved == null);
			let activeMaintenance = selectActiveStatusPageMaintenance(data.activeMaintenances);
			if (shouldFetchComponentMaintenances(data, activeMaintenance)) {
				activeMaintenance = selectActiveStatusPageMaintenance(await this.fetchComponentMaintenances(statusPageUrl));
			}
			runInAction(() => {
				if (statusPageUrl !== getIncidentStatusPageUrl()) {
					return;
				}
				this.consecutiveFailures = 0;
				if (activeIncident) {
					this.incident = {
						id: activeIncident.id,
						name: activeIncident.name,
						status: INCIDENT_STATUS_MAP[activeIncident.status] ?? 'investigating',
						impact: INCIDENT_IMPACT_MAP[activeIncident.impact] ?? 'minor',
						url: activeIncident.url,
						updatedAt: activeIncident.updatedAt ?? activeIncident.updated ?? '',
					};
				} else if (activeMaintenance?.status === 'in_progress') {
					this.incident = {
						id: activeMaintenance.id,
						name: activeMaintenance.name,
						status: 'investigating',
						impact: 'maintenance',
						url: activeMaintenance.url,
						updatedAt: activeMaintenance.start,
					};
				} else {
					this.incident = null;
				}
				this.scheduledMaintenance = activeMaintenance;
			});
		} catch {
			this.recordFailure(statusPageUrl);
		}
	}

	private recordFailure(statusPageUrl: string): void {
		runInAction(() => {
			if (statusPageUrl !== getIncidentStatusPageUrl()) {
				return;
			}
			this.consecutiveFailures += 1;
			logger.warn('Failed to fetch status page');
		});
	}

	private markStatusPageUnsupported(statusPageUrl: string): void {
		runInAction(() => {
			if (statusPageUrl !== getIncidentStatusPageUrl()) {
				return;
			}
			logger.warn('Stopped polling the status page because it is not an Instatus page');
			this.unsupportedStatusPageUrl = statusPageUrl;
			this.incident = null;
			this.scheduledMaintenance = null;
			this.stopActivePolling();
		});
	}

	private async fetchComponentMaintenances(statusPageUrl: string): Promise<Array<InstatusMaintenance>> {
		try {
			const response = await fetch(`${statusPageUrl}/components.json`, statusPageFetchOptions());
			if (!response.ok) {
				return [];
			}
			const data: InstatusComponentsResponse = await response.json();
			return extractComponentMaintenances(data);
		} catch {
			logger.warn('Failed to fetch status page components');
			return [];
		}
	}

	refreshForConnectionIssue(): void {
		this.refreshIfStale();
	}

	private async refreshAndReschedule(): Promise<void> {
		try {
			await this.checkIncidents();
		} finally {
			this.schedulePoll();
		}
	}

	private refreshNow(): void {
		this.clearPollTimer();
		void this.refreshAndReschedule();
	}

	private refreshIfStale(): void {
		if (document.visibilityState !== 'visible') {
			return;
		}
		const staleAfter =
			this.consecutiveFailures > 0 ? computeRetryDelay(this.consecutiveFailures) : POLL_RESUME_STALE_MS;
		if (Date.now() - this.lastCheckedAt < staleAfter) {
			return;
		}
		this.refreshNow();
	}

	private handleVisibilityChange(): void {
		this.refreshIfStale();
	}

	private handleWindowResume(): void {
		this.refreshIfStale();
	}

	private addResumeListeners(): void {
		document.addEventListener('visibilitychange', this.handleVisibilityChange);
		window.addEventListener('focus', this.handleWindowResume);
		window.addEventListener('online', this.handleWindowResume);
	}

	private removeResumeListeners(): void {
		document.removeEventListener('visibilitychange', this.handleVisibilityChange);
		window.removeEventListener('focus', this.handleWindowResume);
		window.removeEventListener('online', this.handleWindowResume);
	}

	private clearPollTimer(): void {
		if (this.pollTimerId === null) {
			return;
		}
		clearTimeout(this.pollTimerId);
		this.pollTimerId = null;
	}
}

export default new StatusPage();
