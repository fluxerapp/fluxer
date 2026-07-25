import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:fluxer_app/features/events/data/events_repository.dart';
import 'package:fluxer_app/features/events/domain/event.dart';

/// Notifier that manages the list of events for a single calendar channel.
///
/// It exposes CRUD operations and handles optimistic updates so the UI
/// responds instantly without waiting for the server round-trip.
class ChannelEventsNotifier
    extends AutoDisposeFamilyAsyncNotifier<List<GuildEvent>, String> {
  @override
  Future<List<GuildEvent>> build(String channelId) async {
    return ref.read(eventsRepositoryProvider).listEvents(channelId);
  }

  /// Refreshes the event list from the server.
  Future<void> refresh() async {
    state = const AsyncValue.loading();
    state = await AsyncValue.guard(
      () => ref.read(eventsRepositoryProvider).listEvents(arg),
    );
  }

  /// Adds a newly created event to the list (called after creation API call).
  void addEvent(GuildEvent event) {
    state.whenData((events) {
      state = AsyncValue.data([...events, event]);
    });
  }

  /// Updates an event in the list (called after edit API call or via gateway).
  void updateEvent(GuildEvent updated) {
    state.whenData((events) {
      state = AsyncValue.data([
        for (final e in events)
          if (e.id == updated.id) updated else e,
      ]);
    });
  }

  /// Removes an event from the list (called after delete API call or gateway).
  void removeEvent(String eventId) {
    state.whenData((events) {
      state = AsyncValue.data(
        events.where((e) => e.id != eventId).toList(),
      );
    });
  }

  /// Optimistically toggles the RSVP state, then syncs with server.
  Future<void> toggleRsvp(String eventId) async {
    // Optimistic update
    state.whenData((events) {
      state = AsyncValue.data([
        for (final e in events)
          if (e.id == eventId)
            e.copyWith(
              isAttending: !e.isAttending,
              attendeeCount: e.isAttending
                  ? (e.attendeeCount - 1).clamp(0, 999999)
                  : e.attendeeCount + 1,
            )
          else
            e,
      ]);
    });

    try {
      final updated = await ref
          .read(eventsRepositoryProvider)
          .toggleRsvp(arg, eventId);
      updateEvent(updated);
    } catch (_) {
      // Roll back optimistic change on failure
      await refresh();
      rethrow;
    }
  }

  /// Creates a new event on the server and adds it to local state.
  Future<GuildEvent> createEvent({
    required String name,
    required DateTime startsAt,
    required DateTime endsAt,
    String? description,
    String? locationChannelId,
    String? locationText,
    EventRepeatType repeatType = EventRepeatType.never,
    int repeatInterval = 1,
  }) async {
    final event = await ref.read(eventsRepositoryProvider).createEvent(
          channelId: arg,
          name: name,
          startsAt: startsAt,
          endsAt: endsAt,
          description: description,
          locationChannelId: locationChannelId,
          locationText: locationText,
          repeatType: repeatType,
          repeatInterval: repeatInterval,
        );
    addEvent(event);
    return event;
  }

  /// Deletes an event on the server and removes it from local state.
  Future<void> deleteEvent(String eventId) async {
    await ref.read(eventsRepositoryProvider).deleteEvent(arg, eventId);
    removeEvent(eventId);
  }
}

final channelEventsProvider = AsyncNotifierProvider.autoDispose
    .family<ChannelEventsNotifier, List<GuildEvent>, String>(
  ChannelEventsNotifier.new,
);

/// Provider that returns events grouped by their calendar date (local time).
///
/// Returns a map of [DateTime] (midnight local) -> [List<GuildEvent>].
final calendarEventsByDateProvider = Provider.autoDispose
    .family<Map<DateTime, List<GuildEvent>>, String>((ref, channelId) {
  final eventsAsync = ref.watch(channelEventsProvider(channelId));
  return eventsAsync.whenOrNull(
        data: (events) {
          final grouped = <DateTime, List<GuildEvent>>{};
          for (final event in events) {
            final day = DateTime(
              event.startsAt.toLocal().year,
              event.startsAt.toLocal().month,
              event.startsAt.toLocal().day,
            );
            grouped.putIfAbsent(day, () => <GuildEvent>[]).add(event);
          }
          return grouped;
        },
      ) ??
      {};
});

/// Provider for attendees of a specific event.
final eventAttendeesProvider = FutureProvider.autoDispose
    .family<List<EventAttendee>, (String, String)>((ref, args) {
  final (channelId, eventId) = args;
  return ref.read(eventsRepositoryProvider).listAttendees(channelId, eventId);
});
