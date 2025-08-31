// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::fmt::{self, Write as _};
use std::ops::{Deref, DerefMut};

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NodeId(pub usize);

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EdgeId(pub usize);

struct DiNode<N> {
    pub id: NodeId,
    pub value: N,
}

impl<N> Deref for DiNode<N> {
    type Target = N;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<N> DerefMut for DiNode<N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

struct DiEdge<E> {
    pub id: EdgeId,
    pub src: NodeId,
    pub dst: NodeId,
    pub value: E,
}

impl<E> Deref for DiEdge<E> {
    type Target = E;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<E> DerefMut for DiEdge<E> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

pub struct DiGraph<N, E> {
    nodes: Vec<DiNode<N>>,
    edges: Vec<DiEdge<E>>,
}

impl<N, E> DiGraph<N, E> {
    pub fn new() -> Self {
        Self { nodes: Vec::new(), edges: Vec::new() }
    }

    pub fn add_node(&mut self, value: N) -> NodeId {
        let id = NodeId(self.nodes.len());
        self.nodes.push(DiNode { id, value });
        id
    }

    pub fn add_edge(&mut self, src: NodeId, dst: NodeId, value: E) -> EdgeId {
        let id = EdgeId(self.edges.len());
        self.edges.push(DiEdge { id, src, dst, value });
        id
    }

    pub fn iter_nodes(&self) -> impl Iterator<Item = &DiNode<N>> {
        self.nodes.iter()
    }

    pub fn iter_nodes_mut(&mut self) -> impl Iterator<Item = &mut DiNode<N>> {
        self.nodes.iter_mut()
    }

    pub fn iter_edges(&self) -> impl Iterator<Item = &DiEdge<E>> {
        self.edges.iter()
    }

    pub fn iter_edges_mut(&mut self) -> impl Iterator<Item = &mut DiEdge<E>> {
        self.edges.iter_mut()
    }

    pub fn iter_edges_with_src_mut<'a>(
        &'a mut self,
    ) -> impl Iterator<Item = (&'a mut DiEdge<E>, &'a mut DiNode<N>)> + use<'a, N, E> {
        let nodes = &mut self.nodes as *mut Vec<DiNode<N>>;
        self.edges.iter_mut().filter_map(move |edge| {
            let nodes = unsafe { &mut *nodes };
            let node = nodes.get_mut(edge.src.0)?;
            Some((edge, node))
        })
    }

    pub fn get_node_mut(&mut self, id: NodeId) -> Option<&mut DiNode<N>> {
        self.nodes.get_mut(id.0)
    }

    pub fn get_edge_mut(&mut self, id: EdgeId) -> Option<&mut DiEdge<E>> {
        self.edges.get_mut(id.0)
    }

    pub fn get_edges_from(&self, node: NodeId) -> impl Iterator<Item = &DiEdge<E>> {
        self.edges.iter().filter(move |edge| edge.src == node)
    }

    pub fn sort_by_src(&mut self) {
        self.edges.sort_by_key(|edge| edge.src.0);
    }
}

impl<N, E> DiGraph<N, E>
where
    N: fmt::Debug,
    E: fmt::Debug,
{
    pub fn format_as_mermaid(&mut self, title: &str) -> String {
        fn escape(s: &str) -> String {
            let mut res = String::with_capacity(s.len());
            for c in s.chars() {
                match c {
                    '\n' => res.push_str(r#"\\n"#),
                    '\t' => res.push_str(r#"\\t"#),
                    '"' => res.push_str("&quot;"),
                    '\\' => res.push_str(r#"\\"#),
                    _ => res.push(c),
                }
            }
            res
        }

        self.sort_by_src();

        let mut output = String::new();
        let mut visited = vec![false; self.nodes.len()];

        _ = writeln!(
            &mut output,
            "\
---
title: {}
config:
  layout: elk
  elk:
    considerModelOrder: NONE
---
flowchart TD
",
            title
        );

        for e in &self.edges {
            if let Some(n) = visited.get_mut(e.src.0) {
                *n = true;
                let n = &self.nodes[e.src.0];
                _ = writeln!(
                    &mut output,
                    "    {}[\"{}\"]",
                    e.src.0,
                    escape(&format!("{:?}", n.value))
                );
            }
            _ = writeln!(
                &mut output,
                "    {} -->|\"{}\"| {}",
                e.src.0,
                escape(&format!("{:?}", e.value)),
                e.dst.0,
            );
        }

        output
    }
}

impl<N, E> Default for DiGraph<N, E> {
    fn default() -> Self {
        Self::new()
    }
}
