
use std::collections::{HashMap, VecDeque};

use rand::prelude::*;


#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum PropertySet {
	Brown,
	LightBlue,
	Pink,
	Orange,
	Red,
	Yellow,
	Green,
	DarkBlue,
	Station,
	Utility,
}
impl PropertySet {
	pub fn all() -> &'static [PropertySet] {
		use crate::PropertySet::*;
		&[
			Brown,
			LightBlue,
			Pink,
			Orange,
			Red,
			Yellow,
			Green,
			DarkBlue,
			Station,
			Utility,
		]
	}
	
	pub fn value(&self) -> u32 {
		use crate::PropertySet::*;
		match self {
			Brown => 1,
			LightBlue => 1,
			Pink => 2,
			Orange => 2,
			Red => 3,
			Yellow => 3,
			Green => 4,
			DarkBlue => 4,
			Station => 2,
			Utility => 2,
		}
	}
	
	pub fn properties(&self) -> &'static [Property] {
		use crate::{Property::*, PropertySet::*};
		match self {
			Brown => &[
				OldKentRoad,
				WhitechapelRoad
			],
			LightBlue => &[
				EustonRoad,
				PentonvilleRoad,
				TheAngelIslington
			],
			Pink => &[
				NorthumberlandAvenue,
				Whitehall,
				PallMall
			],
			Orange => &[
				MarlboroughStreet,
				VineStreet,
				BowStreet
			],
			Red => &[
				Strand,
				TrafalgarSquare,
				FleetStreet
			],
			Yellow => &[
				Piccadilly,
				CoventryStreet,
				LeicesterSquare
			],
			Green => &[
				OxfordStreet,
				RegentStreet,
				BondStreet
			],
			DarkBlue => &[
				ParkLane,
				Mayfair
			],
			Station => &[
				LiverpoolStStation,
				MaryleboneStation,
				FenchurchStStation,
				KingsCrossStation
			],
			Utility => &[
				ElectricCompany,
				WaterWorks
			],
		}
	}
	
	pub fn len(&self) -> usize {
		self.properties().len()
	}
	
	pub fn rents(&self) -> &'static [u32] {
		use crate::PropertySet::*;
		match self {
			Brown     => &[1, 2],
			LightBlue => &[1, 2, 3],
			Pink      => &[1, 2, 4],
			Orange    => &[1, 3, 5],
			Red       => &[2, 3, 6],
			Yellow    => &[2, 4, 6],
			Green     => &[2, 4, 7],
			DarkBlue  => &[3, 8],
			Station   => &[1, 2, 3, 4],
			Utility   => &[1, 2],
		}
	}
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum MoneyCard {
	V1,
	V2,
	V3,
	V4,
	V5,
	V10,
}
impl MoneyCard {
	pub fn value(&self) -> u32 {
		use MoneyCard::*;
		match self {
			V1 => 1,
			V2 => 2,
			V3 => 3,
			V4 => 4,
			V5 => 5,
			V10 => 10,
		}
	}
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum ActionCard {
	PassGo,
	Birthday,
	DebtCollector,
	DoubleTheRent,
	Rent(PropertySet, PropertySet),
	RentAny,
	House,
	Hotel,
	ForcedDeal,
	SlyDeal,
	No,
	DealBreaker,
}
impl ActionCard {
	fn value(&self) -> u32 {
		use ActionCard::*;
		match self {
			PassGo => 1,
			Birthday => 2,
			DebtCollector => 3,
			DoubleTheRent => 1,
			Rent(_, _) => 1,
			RentAny => 3,
			House => 3,
			Hotel => 4,
			ForcedDeal => 3,
			SlyDeal => 3,
			No => 4,
			DealBreaker => 5,
		}
	}
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Property {
	// Brown
	OldKentRoad,
	WhitechapelRoad,
	// LightBlue
	EustonRoad,
	PentonvilleRoad,
	TheAngelIslington,
	// Pink
	NorthumberlandAvenue,
	Whitehall,
	PallMall,
	// Orange
	MarlboroughStreet,
	VineStreet,
	BowStreet,
	// Red
	Strand,
	TrafalgarSquare,
	FleetStreet,
	// Yellow
	Piccadilly,
	CoventryStreet,
	LeicesterSquare,
	// Green
	OxfordStreet,
	RegentStreet,
	BondStreet,
	// DarkBlue
	ParkLane,
	Mayfair,
	// Station
	LiverpoolStStation,
	MaryleboneStation,
	FenchurchStStation,
	KingsCrossStation,
	// Utility
	ElectricCompany,
	WaterWorks,
}
impl Property {
	pub fn set(&self) -> PropertySet {
		use crate::{Property::*, PropertySet::*};
		match self {
			OldKentRoad | WhitechapelRoad => Brown,
			EustonRoad | PentonvilleRoad | TheAngelIslington => LightBlue,
			NorthumberlandAvenue | Whitehall | PallMall => Pink,
			MarlboroughStreet | VineStreet | BowStreet => Orange,
			Strand | TrafalgarSquare | FleetStreet => Red,
			Piccadilly | CoventryStreet | LeicesterSquare => Yellow,
			OxfordStreet | RegentStreet | BondStreet => Green,
			ParkLane | Mayfair => DarkBlue,
			LiverpoolStStation | MaryleboneStation | FenchurchStStation | KingsCrossStation => Station,
			ElectricCompany | WaterWorks => Utility,
		}
	}
	
	pub fn value(&self) -> u32 {
		self.set().value()
	}
	
	pub fn all() -> &'static [Property] {
		use crate::Property::*;
		&[
			// Brown
			OldKentRoad,
			WhitechapelRoad,
			// LightBlue
			EustonRoad,
			PentonvilleRoad,
			TheAngelIslington,
			// Pink
			NorthumberlandAvenue,
			Whitehall,
			PallMall,
			// Orange
			MarlboroughStreet,
			VineStreet,
			BowStreet,
			// Red
			Strand,
			TrafalgarSquare,
			FleetStreet,
			// Yellow
			Piccadilly,
			CoventryStreet,
			LeicesterSquare,
			// Green
			OxfordStreet,
			RegentStreet,
			BondStreet,
			// DarkBlue
			ParkLane,
			Mayfair,
			// Station
			LiverpoolStStation,
			MaryleboneStation,
			FenchurchStStation,
			KingsCrossStation,
			// Utility
			ElectricCompany,
			WaterWorks,
		]
	}
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum PropertyCard {
	Wildcard(PropertySet, PropertySet),
	WildcardAny,
	Property(Property),
	House,
	Hotel,
}
impl From<Property> for PropertyCard {
	fn from(p: Property) -> PropertyCard {
		PropertyCard::Property(p)
	}
}
impl PropertyCard {
	fn value(&self) -> u32 {
		fn wildcard_value(x: PropertySet, y: PropertySet) -> Option<u32> {
			use crate::PropertySet::*;
			match (x, y) {
				(LightBlue, Brown) => Some(1),
				(Pink, Orange) => Some(2),
				(Utility, Station) => Some(2),
				(Red, Yellow) => Some(3),
				(Green, Station) => Some(4),
				(Green, DarkBlue) => Some(4),
				(Station, LightBlue) => Some(4),
				_ => None,
			}
		}
		match *self {
			PropertyCard::Wildcard(x, y) => {
				wildcard_value(x, y)
					.or_else(|| wildcard_value(y, x))
					.unwrap_or(0)
			},
			PropertyCard::WildcardAny => 0,
			PropertyCard::Property(p) => p.value(),
		}
	}
	
	pub fn is_wildcard(&self) -> bool {
		match self {
			PropertyCard::Wildcard(_, _) => true,
			PropertyCard::WildcardAny => true,
			PropertyCard::Property(_) => false,
			PropertyCard::House => false,
			PropertyCard::Hotel => false,
		}
	}
	
	pub fn is_property(&self) -> bool {
		match self {
			PropertyCard::Wildcard(_, _) => false,
			PropertyCard::WildcardAny => false,
			PropertyCard::Property(_) => true,
			PropertyCard::House => false,
			PropertyCard::Hotel => false,
		}
	}
	
	pub fn is_house_or_hotel(&self) -> bool {
		match self {
			PropertyCard::Wildcard(_, _) => false,
			PropertyCard::WildcardAny => false,
			PropertyCard::Property(_) => false,
			PropertyCard::House => true,
			PropertyCard::Hotel => true,
		}
	}
	
	pub fn is_stealable(&self) -> bool {
		match self {
			PropertyCard::Wildcard(_, _) => true,
			PropertyCard::WildcardAny => true,
			PropertyCard::Property(_) => true,
			PropertyCard::House => false,
			PropertyCard::Hotel => false,
		}
	}
}

pub const MAX_CARDS_IN_HAND: usize = 7;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Card {
	MoneyCard(MoneyCard),
	ActionCard(ActionCard),
	PropertyCard(PropertyCard),
}
impl From<MoneyCard> for Card {
	fn from(card: MoneyCard) -> Card {
		Card::MoneyCard(card)
	}
}
impl From<ActionCard> for Card {
	fn from(card: ActionCard) -> Card {
		Card::ActionCard(card)
	}
}
impl From<PropertyCard> for Card {
	fn from(card: PropertyCard) -> Card {
		Card::PropertyCard(card)
	}
}
impl Card {
	pub fn value(&self) -> u32 {
		use Card::*;
		match self {
			MoneyCard(m) => m.value(),
			ActionCard(m) => m.value(),
			PropertyCard(m) => m.value(),
		}
	}
}

fn initial_deck_unshuffled() -> Vec<Card> {
	use crate::MoneyCard::*;
	use crate::ActionCard::*;
	use crate::PropertySet::*;
	
	let deck: Vec<(usize, Card)> = vec![
		(6, V1.into()),
		(5, V2.into()),
		(3, V3.into()),
		(3, V4.into()),
		(2, V5.into()),
		(1, V10.into()),
		
		(10, PassGo.into()),
		(3, Birthday.into()),
		(3, DebtCollector.into()),
		(2, DoubleTheRent.into()),
		(2, Rent(Brown, LightBlue).into()),
		(2, Rent(Station, Utility).into()),
		(2, Rent(Green, DarkBlue).into()),
		(2, Rent(Red, Yellow).into()),
		(2, Rent(Pink, Orange).into()),
		(3, RentAny.into()),
		(3, House.into()),
		(2, Hotel.into()),
		(3, ForcedDeal.into()),
		(3, SlyDeal.into()),
		(3, No.into()),
		(2, DealBreaker.into()),
		
		(2, PropertyCard::Wildcard(Red, Yellow).into()),
		(2, PropertyCard::Wildcard(Pink, Orange).into()),
		(1, PropertyCard::Wildcard(Station, Utility).into()),
		(1, PropertyCard::Wildcard(Green, Station).into()),
		(1, PropertyCard::Wildcard(LightBlue, Station).into()),
		(1, PropertyCard::Wildcard(LightBlue, Brown).into()),
		(1, PropertyCard::Wildcard(DarkBlue, Green).into()),
		(2, PropertyCard::WildcardAny.into()),
	];
	let mut deck: Vec<Card> = deck.into_iter().flat_map(|(n, e)| std::iter::repeat(e).take(n)).collect();
	deck.extend(Property::all().iter().map(|&p| Card::from(PropertyCard::from(p))));
	deck
}

fn initial_deck() -> Vec<Card> {
	let mut deck = initial_deck_unshuffled();
	deck.shuffle(&mut thread_rng());
	deck
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ActionCardAction {
	/// Draw 2 extra cards.
	PassGo,
	/// Collect $2 from every player.
	Birthday,
	/// Collect $5 from the specified player.
	DebtCollector{pid: usize},
	/// Collect rent for the specified property set from all players.
	/// Second argument is the number of `DoubleTheRent` cards used with this rent card.
	Rent{set: PropertySet, double_rents: usize},
	/// Force one player to pay you rent for properties in the set specified.
	RentAny{set: PropertySet, pid: usize, double_rents: usize},
	/// Add a house onto any full set to add $3 to the rent value.
	/// 
	/// Excludes stations and utilities.
	House{set: PropertySet},
	/// Add onto any full set you own to add $4 to the rent value.
	/// 
	/// Excludes stations and utilities.
	Hotel{set: PropertySet},
	/// Swap any property with another player. Cannot be part of a full set.
	ForcedDeal{
		my_set: PropertySet,
		my_card: usize,
		pid: usize,
		other_set: PropertySet,
		other_card: usize
	},
	/// Steal a property from another player.
	SlyDeal{pid: usize, set: PropertySet, card: usize},
	/// Steal a complete set of properties from a player.
	DealBreaker{pid: usize, set: PropertySet},
}

/// Maximum number of actions allowed to be taken per turn
pub const MAX_ACTIONS: usize = 3;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Action {
	/// Build property
	BuildProperty(usize, PropertySet),
	/// Take action card
	TakeActionCard(usize, ActionCardAction),
	/// Move card to bank (either action or money card)
	MoveToBank(usize),
	/// Move property wildcard
	MovePropertyWildcard((PropertySet, usize), PropertySet),
	/// Discard card
	Discard(usize),
	/// Skip turn
	Skip,
}
impl Action {
	pub fn is_free_action(&self) -> bool {
		match self {
			Action::BuildProperty(_, _) => false,
			Action::TakeActionCard(_, _) => false,
			Action::MoveToBank(_) => false,
			
			Action::MovePropertyWildcard(_, _) => true,
			Action::Discard(_) => true,
			Action::Skip => true,
		}
	}
}

pub trait Ai: AiClone + std::fmt::Debug {
	fn should_use_nope(&mut self, state: &GameState, action: ActionCardAction) -> bool;
	fn think(&mut self, state: &GameState) -> Action;
}

pub trait AiClone {
	fn clone_box(&self) -> Box<Ai>;
}

impl<T: 'static + Ai + Clone> AiClone for T {
	fn clone_box(&self) -> Box<Ai> {
		Box::new(self.clone())
	}
}

impl Clone for Box<Ai> {
	fn clone(&self) -> Box<Ai> {
		self.clone_box()
	}
}

#[derive(Clone, Debug)]
pub struct RandomAi {
	pub pid: usize,
}
impl RandomAi {
	pub fn new(pid: usize) -> RandomAi {
		RandomAi {
			pid
		}
	}
}
impl Ai for RandomAi {
	fn should_use_nope(&mut self, state: &GameState, action: ActionCardAction) -> bool {
		random()
	}
	fn think(&mut self, state: &GameState) -> Action {
		*state.get_valid_actions().choose(&mut thread_rng()).expect("no valid actions")
	}
}

#[derive(Clone, Debug)]
pub struct Player {
	pub id: usize,
	pub hand: Vec<Card>,
	pub bank: Vec<Card>,
	pub properties: HashMap<PropertySet, Vec<PropertyCard>>,
}
impl Player {
	pub fn new(id: usize) -> Player {
		Player {
			id,
			hand: Vec::new(),
			bank: Vec::new(),
			properties: HashMap::new(),
		}
	}
	
	pub fn num_full_sets(&self) -> usize {
		let mut sets = 0;
		for (set, cards) in self.properties.iter() {
			if cards.len() >= set.len() {
				sets += 1;
			}
		}
		sets
	}
	
	pub fn num_double_rent_cards(&self) -> usize {
		self.hand.iter()
			.filter(|&&c| c == Card::ActionCard(ActionCard::DoubleTheRent))
			.count()
	}
	
	pub fn is_property_set_nonempty(&self, set: &PropertySet) -> bool {
		if let Some(cards) = self.properties.get(set) {
			cards.iter().filter(|c| c.is_property()).count() > 0
		} else {
			false
		}
	}
	
	pub fn is_property_set_full(&self, set: &PropertySet) -> bool {
		if let Some(cards) = self.properties.get(set) {
			// Not all non-property cards && number of non-house/hotel cards >= property set length
			!cards.iter().all(|c| !c.is_property())
				&& cards.iter().filter(|c| !c.is_house_or_hotel()).count() >= set.len()
		} else {
			false
		}
	}
	
	pub fn nonempty_property_sets<'a>(&'a self) -> impl Iterator<Item=PropertySet> + 'a {
		self.properties.keys()
			.filter(|set| self.is_property_set_nonempty(set))
			.cloned()
	}
	
	pub fn full_property_sets<'a>(&'a self) -> impl Iterator<Item=PropertySet> + 'a {
		self.properties.keys()
			.filter(|set| self.is_property_set_full(set))
			.cloned()
	}
	
	pub fn stealable_cards<'a>(&'a self) -> impl Iterator<Item=(PropertySet, usize)> + 'a {
		self.properties.keys()
			.filter(|set| self.is_property_set_nonempty(set))
			.filter(|set| !self.is_property_set_full(set))
			.flat_map(|set|
				self.properties[set].iter()
					.enumerate()
					.filter(|(i, card)| card.is_stealable())
					.map(|(i, _)| (*set, i))
			)
	}
}

#[derive(Clone, Debug)]
pub struct GameState {
	pub current_player: usize,
	pub actions_taken: usize,
	pub players: Vec<Player>,
	pub deck: VecDeque<Card>,
}
impl GameState {
	pub fn get_valid_actions(&self) -> Vec<Action> {
		let mut actions = Vec::new();
		let p = &self.players[self.current_player];
		let other_pids = (0..self.players.len()).filter(|&pid| pid != self.current_player);
		
		// Add default actions
		actions.push(Action::Skip);
		if p.hand.len() > 7 {
			actions.extend((0..p.hand.len()).map(|i| Action::Discard(i)));
		}
		
		// Move property wildcard
		for (&set, cards) in p.properties.iter() {
			for (i, card) in cards.iter().enumerate() {
				match card {
					PropertyCard::Wildcard(set_a, set_b) => {
						p.properties.keys()
							.filter(|&set_to| *set_to != set && (set_to == set_a || set_to == set_b))
							.map(|&set_to| Action::MovePropertyWildcard((set, i), set_to))
							.for_each(|a| actions.push(a));
					},
					PropertyCard::WildcardAny => {
						p.properties.keys()
							.filter(|&set_to| *set_to != set)
							.map(|&set_to| Action::MovePropertyWildcard((set, i), set_to))
							.for_each(|a| actions.push(a));
					},
					PropertyCard::Property(_) => {}
				}
			}
		}
		
		if self.actions_taken >= MAX_ACTIONS {
			// Only return free actions when number of actions taken is >= MAX_ACTIONS.
			return actions;
		}
		
		let num_double_rents = p.num_double_rent_cards();
		
		// Add actions for each card in hand
		for (i, card) in p.hand.iter().enumerate() {
			match card {
				Card::MoneyCard(_) => actions.push(Action::MoveToBank(i)),
				Card::ActionCard(card) => {
					actions.push(Action::MoveToBank(i));
					match card {
						ActionCard::PassGo =>
							actions.push(Action::TakeActionCard(i, ActionCardAction::PassGo)),
						ActionCard::Birthday =>
							actions.push(Action::TakeActionCard(i, ActionCardAction::Birthday)),
						ActionCard::DebtCollector => {
							other_pids.clone().for_each(|pid| {
								actions.push(Action::TakeActionCard(i, ActionCardAction::DebtCollector{pid}));
							});
						},
						ActionCard::DoubleTheRent => {},
						ActionCard::Rent(set_a, set_b) => {
							if p.is_property_set_nonempty(set_a) {
								for double_rents in 0..=num_double_rents {
									actions.push(Action::TakeActionCard(i, ActionCardAction::Rent{set: *set_a, double_rents}));
								}
							}
							if p.is_property_set_nonempty(set_b) {
								for double_rents in 0..=num_double_rents {
									actions.push(Action::TakeActionCard(i, ActionCardAction::Rent{set: *set_b, double_rents}));
								}
							}
						},
						ActionCard::RentAny => {
							for set in p.nonempty_property_sets() {
								for pid in other_pids.clone() {
									for double_rents in 0..=num_double_rents {
										actions.push(Action::TakeActionCard(i, ActionCardAction::RentAny{set, pid, double_rents}));
									}
								}
							}
						},
						ActionCard::House => {
							for set in p.full_property_sets() {
								actions.push(Action::TakeActionCard(i, ActionCardAction::House{set}))
							}
						},
						ActionCard::Hotel => {
							for set in p.full_property_sets() {
								actions.push(Action::TakeActionCard(i, ActionCardAction::Hotel{set}))
							}
						},
						ActionCard::ForcedDeal => {
							for (my_set, my_card) in p.stealable_cards() {
								for pid in other_pids.clone() {
									for (other_set, other_card) in self.players[pid].stealable_cards() {
										actions.push(Action::TakeActionCard(i, ActionCardAction::ForcedDeal{
											my_set,
											my_card,
											pid,
											other_set,
											other_card
										}));
									}
								}
							}
						},
						ActionCard::SlyDeal => {
							for pid in other_pids.clone() {
								for (set, card) in self.players[pid].stealable_cards() {
									actions.push(Action::TakeActionCard(i, ActionCardAction::SlyDeal{
										pid,
										set,
										card,
									}));
								}
							}
						},
						ActionCard::No => {},
						ActionCard::DealBreaker => {
							for pid in other_pids.clone() {
								for set in self.players[pid].full_property_sets() {
									actions.push(Action::TakeActionCard(i, ActionCardAction::DealBreaker{pid, set}));
								}
							}
						},
					}
				},
				Card::PropertyCard(card) => match card {
					PropertyCard::Wildcard(set_a, set_b) => {
						actions.push(Action::BuildProperty(i, *set_a));
						actions.push(Action::BuildProperty(i, *set_b));
					},
					PropertyCard::WildcardAny => {
						for set in PropertySet::all() {
							actions.push(Action::BuildProperty(i, *set));
						}
					},
					PropertyCard::Property(p) => {
						actions.push(Action::BuildProperty(i, p.set()));
					}
				},
			}
		}
		actions
	}
}

#[derive(Clone, Debug)]
pub struct Game {
	/// Current state of the game
	pub state: GameState,
	/// AIs for each player
	pub ais: Vec<Box<Ai>>,
}
impl Game {
	pub fn new(n: usize) -> Game {
		let mut players = Vec::with_capacity(n);
		let mut ais: Vec<Box<dyn Ai>> = Vec::with_capacity(n);
		for i in 0..n {
			players.push(Player::new(i));
			ais.push(Box::new(RandomAi::new(i)));
		}
		
		let mut game = Game {
			state: GameState {
				current_player: 0,
				actions_taken: 0,
				players,
				deck: initial_deck().into(),
			},
			ais,
		};
		
		// Deal 5 cards to each player
		for p in 0..game.state.players.len() {
			game.dealn(p, 5);
		}
		
		// Deal 2 cards from the deck to the starting player
		game.dealn(game.state.current_player, 2);
		
		game
	}
	
	pub fn dealn(&mut self, player: usize, n: usize) {
		for _ in 0..n {
			self.deal(player);
		}
	}
	
	pub fn deal(&mut self, player: usize) {
		if let Some(card) = self.state.deck.pop_front() {
			self.state.players[player].hand.push(card);
		} else {
			println!("Warning: could not deal a card to player {} as the deck is out of cards", player);
		}
	}
	
	pub fn get_winner(&self) -> Option<usize> {
		for p in 0..self.state.players.len() {
			if self.state.players[p].num_full_sets() >= 3 {
				return Some(p);
			}
		}
		None
	}
	
	pub fn run(&mut self) -> usize {
		loop {
			match self.get_winner() {
				Some(w) => break w,
				None => {},
			}
			self.step();
		}
	}
	
	pub fn step(&mut self) {
		let p = self.state.current_player;
		
		let action = self.ais[p].think(&self.state);
		self.take_action(action);
	}
	
	pub fn take_action(&mut self, action: Action) {
		if !action.is_free_action() {
			self.state.actions_taken += 1;
		}
		
		let p = &mut self.state.players[self.state.current_player];
		
		match action {
			Action::BuildProperty(i, set) => {
				let c = p.hand.remove(i);
				if let Card::PropertyCard(c) = c {
					println!("Built property in set {:?}: {:?}", &set, &c);
					p.properties.entry(set).or_insert_with(|| Vec::new()).push(c);
				} else {
					panic!("invalid card (expected property card): {:?}", &c);
				}
			},
			Action::TakeActionCard(i, action) => {
				unimplemented!()
			},
			Action::MoveToBank(i) => {
				let c = p.hand.remove(i);
				println!("Moved card to bank: {:?}", &c);
				p.bank.push(c);
			},
			Action::MovePropertyWildcard((set_from, i), set_to) => {
				let c = p.properties.get_mut(&set_from).unwrap().remove(i);
				p.properties.entry(set_to).or_insert_with(|| Vec::new()).push(c);
			},
			Action::Discard(i) => {
				let c = p.hand.remove(i);
				println!("Discarded card: {:?}", &c);
				self.state.deck.push_back(c);
			},
			Action::Skip => {
				self.next_player();
			}
		}
	}
	
	pub fn next_player(&mut self) {
		let p = &mut self.state.players[self.state.current_player];
		// Check hand size
		while p.hand.len() > MAX_CARDS_IN_HAND {
			// Discard randomly
			let c = p.hand.remove(thread_rng().gen_range(0, p.hand.len()));
			println!("Discarded card: {:?}", &c);
			self.state.deck.push_back(c);
		}
		
		// Go to next player
		self.state.current_player += 1;
		if self.state.current_player > self.state.players.len() {
			self.state.current_player = 0;
		}
		self.state.actions_taken = 0;
		
		// Deal 2 cards from the deck to the new player
		self.dealn(self.state.current_player, 2);
	}
}


fn main() {
	let mut game = Game::new(4);
	let winner = game.run();
	println!("Winner: {}", winner);
}
