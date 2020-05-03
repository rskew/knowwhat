CREATE TABLE public."edgeMappingEdges" (
    id uuid NOT NULL,
    "mappingId" uuid NOT NULL,
    "sourceEdge" uuid NOT NULL,
    "targetEdge" uuid NOT NULL,
    "midpointAngle" double precision NOT NULL,
    "midpointRadius" double precision NOT NULL,
    deleted boolean NOT NULL
);
CREATE TABLE public.edges (
    id uuid NOT NULL,
    "graphId" uuid NOT NULL,
    source uuid NOT NULL,
    target uuid NOT NULL,
    "midpointAngle" double precision NOT NULL,
    "midpointRadius" double precision NOT NULL,
    text text NOT NULL,
    "isValid" boolean NOT NULL,
    deleted boolean NOT NULL
);
CREATE TABLE public.graphs (
    id uuid NOT NULL,
    title text NOT NULL
);
CREATE TABLE public.mappings (
    id uuid NOT NULL,
    title text NOT NULL,
    "sourceGraph" uuid NOT NULL,
    "targetGraph" uuid NOT NULL
);
CREATE TABLE public."nodeMappingEdges" (
    id uuid NOT NULL,
    "mappingId" uuid NOT NULL,
    "sourceNode" uuid NOT NULL,
    "targetNode" uuid NOT NULL,
    "midpointAngle" double precision NOT NULL,
    "midpointRadius" double precision NOT NULL,
    deleted boolean NOT NULL
);
CREATE TABLE public.nodes (
    id uuid NOT NULL,
    "graphId" uuid NOT NULL,
    subgraph uuid,
    "positionX" double precision NOT NULL,
    "positionY" double precision NOT NULL,
    text text NOT NULL,
    "isValid" boolean NOT NULL,
    deleted boolean NOT NULL
);
CREATE TABLE public."pathEquations" (
    "pathEquationId" uuid NOT NULL,
    "graphId" uuid NOT NULL,
    deleted boolean NOT NULL,
    id text NOT NULL,
    "edgeId" uuid NOT NULL
);
ALTER TABLE ONLY public."edgeMappingEdges"
    ADD CONSTRAINT "edgeMappingEdges_pkey" PRIMARY KEY (id);
ALTER TABLE ONLY public.edges
    ADD CONSTRAINT edges_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.graphs
    ADD CONSTRAINT graphs_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.graphs
    ADD CONSTRAINT graphs_title_key UNIQUE (title);
ALTER TABLE ONLY public.mappings
    ADD CONSTRAINT mappings_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public."nodeMappingEdges"
    ADD CONSTRAINT "nodeMappingEdges_pkey" PRIMARY KEY (id);
ALTER TABLE ONLY public.nodes
    ADD CONSTRAINT nodes_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public."pathEquations"
    ADD CONSTRAINT "pathEquations_pkey" PRIMARY KEY (id);
ALTER TABLE ONLY public."edgeMappingEdges"
    ADD CONSTRAINT "edgeMappingEdges_mappingId_fkey" FOREIGN KEY ("mappingId") REFERENCES public.mappings(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public."edgeMappingEdges"
    ADD CONSTRAINT "edgeMappingEdges_sourceEdge_fkey" FOREIGN KEY ("sourceEdge") REFERENCES public.edges(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public."edgeMappingEdges"
    ADD CONSTRAINT "edgeMappingEdges_targetEdge_fkey" FOREIGN KEY ("targetEdge") REFERENCES public.edges(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.edges
    ADD CONSTRAINT "edges_graphId_fkey" FOREIGN KEY ("graphId") REFERENCES public.graphs(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.edges
    ADD CONSTRAINT edges_source_fkey FOREIGN KEY (source) REFERENCES public.nodes(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.edges
    ADD CONSTRAINT edges_target_fkey FOREIGN KEY (target) REFERENCES public.nodes(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.mappings
    ADD CONSTRAINT "mappings_sourceGraph_fkey" FOREIGN KEY ("sourceGraph") REFERENCES public.graphs(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.mappings
    ADD CONSTRAINT "mappings_targetGraph_fkey" FOREIGN KEY ("targetGraph") REFERENCES public.graphs(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public."nodeMappingEdges"
    ADD CONSTRAINT "nodeMappingEdges_mappingId_fkey" FOREIGN KEY ("mappingId") REFERENCES public.mappings(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public."nodeMappingEdges"
    ADD CONSTRAINT "nodeMappingEdges_sourceNode_fkey" FOREIGN KEY ("sourceNode") REFERENCES public.nodes(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public."nodeMappingEdges"
    ADD CONSTRAINT "nodeMappingEdges_targetNode_fkey" FOREIGN KEY ("targetNode") REFERENCES public.nodes(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.nodes
    ADD CONSTRAINT "nodes_graphId_fkey" FOREIGN KEY ("graphId") REFERENCES public.graphs(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.nodes
    ADD CONSTRAINT nodes_subgraph_fkey FOREIGN KEY (subgraph) REFERENCES public.graphs(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public."pathEquations"
    ADD CONSTRAINT "pathEquations_edgeId_fkey" FOREIGN KEY ("edgeId") REFERENCES public.edges(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public."pathEquations"
    ADD CONSTRAINT "pathEquations_graphId_fkey" FOREIGN KEY ("graphId") REFERENCES public.graphs(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
